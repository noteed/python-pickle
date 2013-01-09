{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Very partial implementation of the Python Pickle Virtual Machine
-- (protocol 2): i.e. parses pickled data into opcodes, then executes the
-- opcodes to construct a (Haskell representation of a) Python object.
module Language.Python.Pickle where

import Control.Applicative ((<$>), (<*), (*>))
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.ByteString as S
import Data.Attoparsec hiding (parse, take)
import qualified Data.Attoparsec as A
import Data.Attoparsec.ByteString.Char8 (decimal, double, signed)
import Data.Int (Int32)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Serialize.Get (getWord16le, getWord32le, getWord64be, runGet)
import Data.Serialize.Put (runPut, putByteString, putWord8, putWord16le, putWord32le, putWord64be, Put)
import Data.Word (Word32, Word64)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)

-- | Parse a pickled object to a list of opcodes.
parse :: S.ByteString -> Either String [OpCode]
parse = parseOnly (many1 $ choice opcodes)

-- | Unpickle (i.e. deserialize) a Python object. Protocols 0, 1, and 2 are
-- supported.
unpickle :: S.ByteString -> Either String Value
unpickle s = do
  xs <- parse s
  unpickle' xs

-- | Pickle (i.e. serialize) a Python object. Protocol 2 is used.
pickle :: Value -> S.ByteString
pickle value = runPut $ do
  putByteString "\128\STX"
  mapM_ serialize . runPickler $ pickle' value
  putByteString "."

----------------------------------------------------------------------
-- Pickle opcodes parser
----------------------------------------------------------------------

-- TODO parsing could be done with a big switch and only cereal,
-- instead of relying on attoparsec's "choice" combinator.

opcodes :: [Parser OpCode]
opcodes =
  [ int, binint, binint1, binint2, long, long1, long4
  , string', binstring, short_binstring
  , none
  , newtrue, newfalse
  , unicode, binunicode
  , float, binfloat
  , empty_list, append, appends, list
  , empty_tuple, tuple, tuple1, tuple2, tuple3
  , empty_dict, dict, setitem, setitems
  , pop, dup, mark, popmark
  , get', binget, long_binget, put', binput, long_binput
  , ext1, ext2, ext4
  , global, reduce, build, inst, obj, newobj
  , proto, stop
  , persid, binpersid
  ]

-- Integers

int, binint, binint1, binint2, long, long1, long4 :: Parser OpCode
int = string "I" *> (INT <$> decimalInt)
binint = string "J" *> (BININT <$> int4)
binint1 = string "K" *> (BININT1 . fromIntegral <$> anyWord8)
binint2 = string "M" *> (BININT2 <$> uint2)
long = string "L" *> (LONG <$> decimalLong)
long1 = string "\138" *> (LONG1 <$> decodeLong1) -- same as \x8a
long4 = string "\139" *> (LONG4 <$> decodeLong4) -- same as \x8b

-- Strings

string', binstring, short_binstring :: Parser OpCode
string' = string "S" *> (STRING <$> stringnl)
binstring = string "T" *> (BINSTRING <$> undefined)
short_binstring = do
  _ <- string "U"
  i <- fromIntegral <$> anyWord8
  s <- A.take i
  return $ SHORT_BINSTRING s

-- None

none :: Parser OpCode
none = string "N" *> return NONE

-- Booleans

newtrue, newfalse :: Parser OpCode
newtrue = string "\136" *> return NEWTRUE -- same as \x88
newfalse = string "\137" *> return NEWFALSE -- same as \x89

-- Unicode strings

unicode, binunicode :: Parser OpCode
unicode = string "V" *> (UNICODE <$> undefined)
binunicode = string "X" *> (BINUNICODE <$> undefined)

-- Floats

float, binfloat :: Parser OpCode
float = string "F" *> (FLOAT <$> doubleFloat)
binfloat = string "G" *> (BINFLOAT <$> float8)

-- Lists

empty_list, append, appends, list :: Parser OpCode
empty_list = string "]" *> return EMPTY_LIST
append = string "a" *> return APPEND
appends = string "e" *> return APPENDS
list = string "l" *> return LIST

-- Tuples

empty_tuple, tuple, tuple1, tuple2, tuple3 :: Parser OpCode
empty_tuple = string ")" *> return EMPTY_TUPLE
tuple = string "t" *> return TUPLE
tuple1 = string "\133" *> return TUPLE1 -- same as \x85
tuple2 = string "\134" *> return TUPLE2 -- same as \x86
tuple3 = string "\135" *> return TUPLE3 -- same as \x87

-- Dictionaries

empty_dict, dict, setitem, setitems :: Parser OpCode
empty_dict = string "}" *> return EMPTY_DICT
dict = string "d" *> return DICT
setitem = string "s" *> return SETITEM
setitems = string "u" *> return SETITEMS

-- Stack manipulation

pop, dup, mark, popmark :: Parser OpCode
pop = string "0" *> return POP
dup = string "2" *> return DUP
mark = string "(" *> return MARK
popmark = string "1" *> return POP_MARK

-- Memo manipulation

get', binget, long_binget, put', binput, long_binput :: Parser OpCode
get' = string "g" *> return GET
binget = string "h" *> (BINGET <$> undefined)
long_binget = string "j" *> (LONG_BINGET <$> undefined)
put' = string "p" *> (PUT <$> decimalInt)
binput = string "q" *> (BINPUT . fromIntegral <$> anyWord8)
long_binput = string "r" *> (LONG_BINPUT <$> undefined)

-- Extension registry (predefined objects)

ext1, ext2, ext4 :: Parser OpCode
ext1 = string "\130" *> (EXT1 <$> undefined) -- same as \x82
ext2 = string "\131" *> (EXT2 <$> undefined) -- same as \x83
ext4 = string "\132" *> (EXT4 <$> undefined) -- same as \x84

-- Various

global, reduce, build, inst, obj, newobj :: Parser OpCode
global = string "c" *> (uncurry GLOBAL <$> undefined)
reduce = string "R" *> return REDUCE
build = string "b" *> return BUILD
inst = string "i" *> (uncurry INST <$> undefined)
obj = string "o" *> return OBJ
newobj = string "\129" *> return NEWOBJ -- same as \x81

-- Machine control

proto, stop :: Parser OpCode
proto = string "\128" *> (PROTO . fromIntegral <$> anyWord8)
stop = string "." *> return STOP

-- Persistent IDs

persid, binpersid :: Parser OpCode
persid = string "P" *> (PERSID <$> undefined)
binpersid = string "Q" *> return BINPERSID

-- Basic parsers

decimalInt :: Parser Int
decimalInt = signed decimal <* string "\n"

-- TODO document the differences with Python's representation.
doubleFloat = double <* string "\n"

decimalLong :: Parser Int
decimalLong = signed decimal <* string "L\n"

decodeLong1 :: Parser Int
decodeLong1 = do
  n <- fromIntegral <$> anyWord8
  if n > 0
    then do
      ns <- A.take n
      let a = fst . fst $ toLong ns
      return $ if S.last ns > 127 then negate $ 256 ^ S.length ns - a else a
    else return 0
  where toLong = S.mapAccumL (\(a, b) w -> ((a + 256 ^ b * fromIntegral w, b + 1), w)) (0, 0)

decodeLong4 = undefined -- TODO

-- TODO escaping not implemented.
stringnl = choice
  [ string "'" *> takeTill (== 39) <* string "'\n"
  ]

float8 :: Parser Double
float8 = do
  w <- runGet getWord64be <$> A.take 8
  case w of
    Left err -> fail err
    Right x -> return $ coerce x
  where
  coerce :: Word64 -> Double
  coerce x = unsafePerformIO $ with x $ \p ->
    peek (castPtr p) :: IO Double

int4 :: Parser Int
int4 = do
  w <- runGet getWord32le <$> A.take 4
  case w of
    Left err -> fail err
    Right x -> return . fromIntegral $ coerce x
  where
  coerce :: Word32 -> Int32
  coerce x = unsafePerformIO $ with x $ \p ->
    peek (castPtr p) :: IO Int32

uint2 :: Parser Int
uint2 = do
  w <- runGet getWord16le <$> A.take 2
  case w of
    Left err -> fail err
    Right x -> return $ fromIntegral x

----------------------------------------------------------------------
-- Pickle opcodes serialization
----------------------------------------------------------------------

serialize :: OpCode -> Put
serialize opcode = case opcode of
  BINPUT i -> putByteString "q" >> putWord8 (fromIntegral i)
  BININT i -> putByteString "J" >> putWord32le (fromIntegral i)
  BININT1 i -> putByteString "K" >> putWord8 (fromIntegral i)
  BININT2 i -> putByteString "M" >> putUint2 i
  LONG1 0 -> putByteString "\138\NUL"
  LONG1 i -> putByteString "\138" >> encodeLong1 i
  BINFLOAT d -> putByteString "G" >> putFloat8 d
  SHORT_BINSTRING s -> do
    putByteString "U"
    putWord8 . fromIntegral $ S.length s
    putByteString s
  EMPTY_DICT -> putByteString "}"
  EMPTY_LIST -> putByteString "]"
  EMPTY_TUPLE -> putByteString ")"
  TUPLE1 -> putByteString "\133"
  TUPLE2 -> putByteString "\134"
  MARK -> putByteString "("
  SETITEM -> putByteString "s"
  SETITEMS -> putByteString "u"
  APPEND -> putByteString "a"
  APPENDS -> putByteString "e"
  x -> error $ "serialize: " ++ show x

putFloat8 :: Double -> Put
putFloat8 d = putWord64be (coerce d)
    where
    coerce :: Double -> Word64
    coerce x = unsafePerformIO $ with x $ \p ->
      peek (castPtr p) :: IO Word64

putUint2 :: Int -> Put
putUint2 d = putWord16le (fromIntegral d)

encodeLong1 :: Int -> Put
encodeLong1 i = do
  -- TODO is it possible to know xs length without really constructing xs?
  let xs = f $ abs i
      f j | j < 256 = [fromIntegral j]
          | otherwise = let (n, r) = j `divMod` 256 in fromIntegral r : f n
      e = 256 ^ length xs
      m = e `div` 2 - 1
  if i < 0
    then do
      if (abs i) > e `div` 2
        then do
          putWord8 (fromIntegral $ length xs + 1)
          mapM_ putWord8 $ f (e + i)
          putWord8 255
        else do
          putWord8 (fromIntegral $ length xs)
          mapM_ putWord8 $ f (e + i)
    else
      if i > e `div` 2 - 1
        then do
          putWord8 (fromIntegral $ length xs + 1)
          mapM_ putWord8 xs
          putWord8 0
        else do
          putWord8 (fromIntegral $ length xs)
          mapM_ putWord8 xs

----------------------------------------------------------------------
-- Pickle opcodes
----------------------------------------------------------------------

data OpCode =
  -- Integers
    INT Int
  | BININT Int
  | BININT1 Int
  | BININT2 Int
  | LONG Int
  | LONG1 Int
  | LONG4 Int

  -- Strings
  | STRING S.ByteString
  | BINSTRING S.ByteString
  | SHORT_BINSTRING S.ByteString

  -- None
  | NONE

  -- Booleans
  | NEWTRUE
  | NEWFALSE

  -- Unicode strings
  | UNICODE S.ByteString -- TODO (use Text ?)
  | BINUNICODE S.ByteString

  -- Floats
  | FLOAT Double
  | BINFLOAT Double

  -- Lists
  | EMPTY_LIST
  | APPEND
  | APPENDS
  | LIST

  -- Tuples
  | EMPTY_TUPLE
  | TUPLE
  | TUPLE1
  | TUPLE2
  | TUPLE3

  -- Dictionaries
  | EMPTY_DICT
  | DICT
  | SETITEM
  | SETITEMS

  -- Stack manipulation
  | POP
  | DUP
  | MARK
  | POP_MARK

  -- Memo manipulation
  | GET
  | BINGET Int
  | LONG_BINGET Int
  | PUT Int
  | BINPUT Int
  | LONG_BINPUT Int

  -- Extension registry (predefined objects)
  | EXT1 Int
  | EXT2 Int
  | EXT4 Int

  -- Various
  | GLOBAL S.ByteString S.ByteString
  | REDUCE
  | BUILD
  | INST S.ByteString S.ByteString
  | OBJ
  | NEWOBJ

  -- Pickle machine control
  | PROTO Int -- in [2..255]
  | STOP

  -- Persistent IDs
  | PERSID S.ByteString
  | BINPERSID
  deriving Show

{-
protocol0 = [INT, LONG, STRING, NONE, UNICODE, FLOAT, APPEND, LIST, TUPLE,
  DICT, SETITEM, POP, DUP, MARK, GET, PUT, GLOBAL, REDUCE, BUILD, INST, STOP,
  PERSID]
protocol1 = [BININT, BININT1, BININT2, BINSTRING, SHORT_BINSTRING, BINUNICODE,
  BINFLOAT, EMPTY_LIST, APPENDS, EMPTY_TUPLE, EMPTY_DICT, SETITEMS, POP_MARK,
  BINGET, LONG_BINGET, BINPUT, LONG_BINPUT, OBJ, BINPERSID]
protocol2 = [LONG1, LONG4, NEWTRUE, NEWFALSE, TUPLE1, TUPLE2, TUPLE3, EXT1,
  EXT2, EXT4, NEWOBJ, PROTO]
-}

----------------------------------------------------------------------
-- Pyhon value representation
----------------------------------------------------------------------

-- Maybe I can call them Py? And Have IsString/Num instances?
data Value =
    Dict (Map Value Value)
  | List [Value]
  | Tuple [Value]
  | BinInt Int
  | BinLong Int
  | BinFloat Double
  | BinString S.ByteString
  | MarkObject -- Urk, not really a value.
  deriving (Eq, Ord, Show)

----------------------------------------------------------------------
-- Pickle machine (opcodes to value)
----------------------------------------------------------------------

unpickle' :: [OpCode] -> Either String Value
unpickle' xs = execute xs [] (IM.empty)

type Stack = [Value]

type Memo = IntMap Value

execute :: [OpCode] -> Stack -> Memo -> Either String Value
execute [] [value] _ = Right value
execute (op:ops) stack memo = case executeOne op stack memo of
  Left err -> Left err
  Right (stack', memo') -> execute ops stack' memo'
execute _ _ _ = Left "`execute` unimplemented"

executePartial :: [OpCode] -> Stack -> Memo -> (Stack, Memo, [OpCode])
executePartial [] stack memo = (stack, memo, [])
executePartial (op:ops) stack memo = case executeOne op stack memo of
  Left _ -> (stack, memo, op:ops)
  Right (stack', memo') -> executePartial ops stack' memo'

executeOne :: OpCode -> Stack -> Memo -> Either String (Stack, Memo)
executeOne EMPTY_DICT stack memo = return (Dict M.empty: stack, memo)
executeOne EMPTY_LIST stack memo = return (List []: stack, memo)
executeOne EMPTY_TUPLE stack memo = return (Tuple []: stack, memo)
executeOne (PUT i) (s:stack) memo = return (s:stack, IM.insert i s memo)
executeOne (BINPUT i) (s:stack) memo = return (s:stack, IM.insert i s memo)
executeOne (INT i) stack memo = return (BinInt i:stack, memo)
executeOne (BININT i) stack memo = return (BinInt i:stack, memo)
executeOne (BININT1 i) stack memo = return (BinInt i:stack, memo)
executeOne (BININT2 i) stack memo = return (BinInt i:stack, memo)
executeOne (LONG i) stack memo = return (BinLong i:stack, memo)
executeOne (LONG1 i) stack memo = return (BinLong i:stack, memo)
executeOne (FLOAT d) stack memo = return (BinFloat d:stack, memo)
executeOne (BINFLOAT d) stack memo = return (BinFloat d:stack, memo)
executeOne (STRING s) stack memo = return (BinString s:stack, memo)
executeOne (SHORT_BINSTRING s) stack memo = return (BinString s:stack, memo)
executeOne MARK stack memo = return (MarkObject:stack, memo)
executeOne TUPLE stack memo = executeTuple [] stack memo
executeOne TUPLE1 (a:stack) memo = return (Tuple [a]:stack, memo)
executeOne TUPLE2 (b:a:stack) memo = return (Tuple [a, b]:stack, memo)
executeOne DICT stack memo = executeDict [] stack memo
executeOne SETITEM stack memo = executeSetitem stack memo
executeOne SETITEMS stack memo = executeSetitems [] stack memo
executeOne LIST stack memo = executeList [] stack memo
executeOne APPEND stack memo = executeAppend stack memo
executeOne APPENDS stack memo = executeAppends [] stack memo
executeOne (PROTO _) stack memo = return (stack, memo)
executeOne STOP stack memo = Right (stack, memo)
executeOne op _ _ = Left $ "Can't execute opcode " ++ show op ++ "."

executeTuple :: Monad m => [Value] -> Stack -> Memo -> m ([Value], Memo)
executeTuple l (MarkObject:stack) memo = return (Tuple l:stack, memo)
executeTuple l (a:stack) memo = executeTuple (a : l) stack memo

executeDict :: Monad m => [(Value, Value)] -> Stack -> Memo -> m ([Value], Memo)
executeDict l (MarkObject:stack) memo = return (l `addToDict` Dict M.empty:stack, memo)
executeDict l (a:b:stack) memo = executeDict ((b, a) : l) stack memo

executeList :: Monad m => [Value] -> Stack -> Memo -> m ([Value], Memo)
executeList l (MarkObject:stack) memo = return (List l:stack, memo)
executeList l (x:stack) memo = executeList (x : l) stack memo

executeSetitem :: Monad m => Stack -> Memo -> m ([Value], Memo)
executeSetitem (v:k:Dict d:stack) memo = return (Dict (M.insert k v d):stack, memo)

executeSetitems :: Monad m => [(Value, Value)] -> Stack -> Memo -> m ([Value], Memo)
executeSetitems l (MarkObject:Dict d:stack) memo = return (l `addToDict` Dict d:stack, memo)
executeSetitems l (a:b:stack) memo = executeSetitems ((b, a) : l) stack memo

executeAppend :: Monad m => Stack -> Memo -> m ([Value], Memo)
executeAppend (x:List xs:stack) memo = return (List (xs ++ [x]):stack, memo)

executeAppends :: Monad m => [Value] -> Stack -> Memo -> m ([Value], Memo)
executeAppends l (MarkObject:List xs:stack) memo = return (List (xs ++ l):stack, memo)
executeAppends l (x:stack) memo = executeAppends (x : l) stack memo

addToDict :: [(Value, Value)] -> Value -> Value
addToDict l (Dict d) = Dict $ foldl' add d l
  where add d' (k, v) = M.insert k v d'

----------------------------------------------------------------------
-- Pickling (value to opcodes)
----------------------------------------------------------------------

newtype Pickler a = Pickler { runP :: WriterT [OpCode] (State (Map Value Int)) a }
  deriving (Monad, MonadWriter [OpCode], MonadState (Map Value Int))

runPickler :: Pickler () -> [OpCode]
runPickler p = evalState (execWriterT (runP p)) M.empty

pickle' :: Value -> Pickler ()
pickle' value = case value of
  Dict d -> pickleDict d
  List xs -> pickleList xs
  Tuple xs -> pickleTuple xs
  BinInt i -> pickleBinInt i
  BinLong i -> pickleBinLong i
  BinFloat d -> pickleBinFloat d
  BinString s -> pickleBinString s
  x -> error $ "TODO: pickle " ++ show x

-- TODO actually lookup values in the map, reusing their key.
binput' :: Value -> Pickler ()
binput' value = do
  i <- gets M.size
  m <- get
  put (M.insert value i m)
  tell [BINPUT i]

pickleDict :: Map Value Value -> Pickler ()
pickleDict d = do
  tell [EMPTY_DICT]
  binput' (Dict M.empty)

  let kvs = M.toList d
  case kvs of
    [] -> return ()
    [(k,v)] -> pickle' k >> pickle' v >> tell [SETITEM]
    _ -> do
      tell [MARK]
      mapM_ (\(k, v) -> pickle' k >> pickle' v) kvs
      tell [SETITEMS]

pickleList :: [Value] -> Pickler ()
pickleList xs = do
  tell [EMPTY_LIST]
  binput' (List [])

  case xs of
    [] -> return ()
    [x] -> pickle' x >> tell [APPEND]
    _ -> do
      tell [MARK]
      mapM_ pickle' xs
      tell [APPENDS]

pickleTuple :: [Value] -> Pickler ()
pickleTuple [] = tell [EMPTY_TUPLE]
pickleTuple [a] = do
  pickle' a
  tell [TUPLE1]
  binput' (Tuple [a])
pickleTuple [a, b] = do
  pickle' a
  pickle' b
  tell [TUPLE2]
  binput' (Tuple [a, b])
pickleTuple _ = error "pickleTuple n TODO"

pickleBinInt :: Int -> Pickler ()
pickleBinInt i | i >= 0 && i < 256 = tell [BININT1 i]
               | i >= 256 && i < 65536 = tell [BININT2 i]
               | otherwise = tell [BININT i]

pickleBinLong :: Int -> Pickler ()
pickleBinLong i = tell [LONG1 i] -- TODO LONG/LONG1/LONG4

-- TODO probably depends on the float range
pickleBinFloat :: Double -> Pickler ()
pickleBinFloat d = do
  tell [BINFLOAT d]

-- TODO depending on the string length, it should not always be a SHORT_BINSTRING
pickleBinString :: S.ByteString -> Pickler ()
pickleBinString s = do
  tell [SHORT_BINSTRING s]
  binput' (BinString s)

----------------------------------------------------------------------
-- Manipulate Values
----------------------------------------------------------------------

dictGet :: Value -> Value -> Either String (Maybe Value)
dictGet (Dict d) v = return $ M.lookup v d
dictGet _ _ = Left "dictGet: not a dict."

dictGet' :: Value -> Value -> Either String Value
dictGet' (Dict d) v = case M.lookup v d of
  Just value -> return value
  Nothing -> Left "dictGet': no such key."
dictGet' _ _ = Left "dictGet': not a dict."

dictGetString :: Value -> S.ByteString -> Either String S.ByteString
dictGetString (Dict d) s = case M.lookup (BinString s) d of
  Just (BinString s') -> return s'
  _ -> Left "dictGetString: not a dict, or no such key."
