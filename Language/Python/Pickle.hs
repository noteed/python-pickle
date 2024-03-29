{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Very partial implementation of the Python Pickle Virtual Machine
-- (protocol 2): i.e. parses pickled data into opcodes, then executes the
-- opcodes to construct a (Haskell representation of a) Python object.
module Language.Python.Pickle where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.ByteString as S
import Data.Attoparsec.ByteString hiding (parse, take)
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString.Char8 (decimal, double, signed)
import Data.Functor (($>), (<&>))
import Data.Int (Int32, Int64)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Serialize.Get (getWord16le, getInt32le, getInt64le, runGet)
import Data.Serialize.IEEE754 (getFloat64be)
import Data.Serialize.Put (runPut, putByteString, putWord8, putWord16le, putWord32le, putWord64be, Put)
import qualified Data.Set as SET
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as T
import Data.Word (Word64)
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
  -- true and false are in fact special cases for the int parser,
  -- It is important they are tried before int.
  [ true, false, int, binint, binint1, binint2, long, long1, long4
  , string', binstring, shortBinstring, binbytes, shortBinbytes, binbytes8, bytearray8
  , none
  , newtrue, newfalse
  , unicode, binunicode, shortBinunicode, binunicode8
  , float, binfloat
  , emptyList, append, appends, list
  , emptyTuple, tuple, tuple1, tuple2, tuple3
  , emptyDict, dict
  , setitem, setitems, emptySet, additems, frozenset
  , pop, dup, mark, popmark, stackGlobal
  , get', binget, longBinget, put', binput, longBinput, memoize
  , ext1, ext2, ext4
  , global, reduce, build, inst, obj, newobj, newobjEx, frame, nextBuffer, readonlyBuffer
  , proto, stop
  , persid, binpersid
  ]

-- Integers

int, binint, binint1, binint2, long, long1, long4 :: Parser OpCode
int = string "I" *> (INT <$> decimalInt)
binint = string "J" *> (BININT . fromIntegral <$> anyInt32)
binint1 = string "K" *> (BININT1 . fromIntegral <$> anyWord8)
binint2 = string "M" *> (BININT2 <$> uint2)
long = string "L" *> (LONG <$> decimalLong)
long1 = string "\138" *> (LONG1 <$> decodeLong1)
long4 = string "\139" *> (LONG4 <$> decodeLong4)

-- Strings

string', binstring, shortBinstring, binbytes, shortBinbytes, binbytes8, bytearray8 :: Parser OpCode
string' = string "S" *> (STRING <$> stringnl)
binstring = readSizedVal "T" anyInt32 BINSTRING
shortBinstring = readSizedVal "U" anyWord8 SHORT_BINSTRING
binbytes = readSizedVal "B" anyInt32 BINBYTES
shortBinbytes = readSizedVal "C" anyWord8 SHORT_BINBYTES
binbytes8 = readSizedVal "\142" anyInt64 BINBYTES8
bytearray8 = readSizedVal "\150" anyInt64 BYTEARRAY8


-- None

none :: Parser OpCode
none = string "N" $> NONE

-- Booleans

true, false, newtrue, newfalse :: Parser OpCode
true = string "I01" $> NEWTRUE
false = string "I00" $> NEWFALSE
newtrue = string "\136" $> NEWTRUE -- same as \x88
newfalse = string "\137" $> NEWFALSE -- same as \x89

-- Unicode strings

unicode, binunicode, shortBinunicode, binunicode8 :: Parser OpCode
unicode = string "V" *> (UNICODE . T.decodeUtf8 <$> stringnl')
binunicode = readSizedVal "X" anyInt32 BINUNICODE
shortBinunicode = readSizedVal "\140" anyWord8 SHORT_BINUNICODE
binunicode8 = readSizedVal "\138" anyInt64 BINUNICODE8

-- Floats

float, binfloat :: Parser OpCode
float = string "F" *> (FLOAT <$> doubleFloat)
binfloat = string "G" *> (BINFLOAT <$> float8)

-- Lists

emptyList, append, appends, list :: Parser OpCode
emptyList = string "]" $> EMPTY_LIST
append = string "a" $> APPEND
appends = string "e" $> APPENDS
list = string "l" $> LIST

-- Tuples

emptyTuple, tuple, tuple1, tuple2, tuple3 :: Parser OpCode
emptyTuple = string ")" $> EMPTY_TUPLE
tuple = string "t" $> TUPLE
tuple1 = string "\133" $> TUPLE1 -- same as \x85
tuple2 = string "\134" $> TUPLE2 -- same as \x86
tuple3 = string "\135" $> TUPLE3 -- same as \x87

-- Dictionaries

emptyDict, dict :: Parser OpCode
emptyDict = string "}" $> EMPTY_DICT
dict = string "d" $> DICT

-- Sets

setitem, setitems, emptySet, additems, frozenset :: Parser OpCode
setitem = string "s" $> SETITEM
setitems = string "u" $> SETITEMS
emptySet = string "\143" $> EMPTY_SET
additems = string "\144" $> ADDITEMS
frozenset = string "\145" $> FROZENSET

-- Stack manipulation

pop, dup, mark, popmark, stackGlobal :: Parser OpCode
pop = string "0" $> POP
dup = string "2" $> DUP
mark = string "(" $> MARK
popmark = string "1" $> POP_MARK
stackGlobal = string "\147" $> STACK_GLOBAL

-- Memo manipulation

get', binget, longBinget, put', binput, longBinput, memoize :: Parser OpCode
get' = string "g" *> (GET <$> (signed decimal <* string "\n"))
binget = string "h" *> (BINGET . fromIntegral <$> anyWord8)
longBinget = string "j" *> (LONG_BINGET . fromIntegral <$> anyInt32)
put' = string "p" *> (PUT <$> (signed decimal <* string "\n"))
binput = string "q" *> (BINPUT . fromIntegral <$> anyWord8)
longBinput = string "r" *> (LONG_BINPUT . fromIntegral <$> anyInt32)
memoize = string "\148" $> MEMOIZE

-- Extension registry (predefined objects)

ext1, ext2, ext4 :: Parser OpCode
ext1 = string "\130" *> (EXT1 . fromIntegral <$> anyWord8)
ext2 = string "\131" *> (EXT2 . fromIntegral <$> uint2)
ext4 = string "\132" *> (EXT4 . fromIntegral <$> anyInt32)

-- Various

global, reduce, build, inst, obj, newobj, newobjEx, frame, nextBuffer, readonlyBuffer :: Parser OpCode
global = string "c" *> (GLOBAL <$> stringnl' <*> stringnl')
reduce = string "R" $> REDUCE
build = string "b" $> BUILD
inst = string "i" *> (uncurry INST <$> undefined)
obj = string "o" $> OBJ
newobj = string "\129" $> NEWOBJ -- same as \x81
newobjEx = string "\146" $> NEWOBJ_EX
frame = string "\149" *> (FRAME <$> anyInt64)
nextBuffer = string "\151" $> NEXT_BUFFER
readonlyBuffer = string "\152" $> READONLY_BUFFER

-- Machine control

proto, stop :: Parser OpCode
proto = string "\128" *> (PROTO . fromIntegral <$> anyWord8)
stop = string "." $> STOP

-- Persistent IDs

persid, binpersid :: Parser OpCode
persid = string "P" *> (PERSID <$> stringnl')
binpersid = string "Q" $> BINPERSID

-- Basic parsers

readSizedVal :: (Integral i) => S.ByteString -> Parser i -> (S.ByteString -> r) -> Parser r
readSizedVal opcode sizeParser constructor = do
  _ <- string opcode
  i <- fromIntegral <$> sizeParser
  s <- A.take i
  return $ constructor s

decimalInt :: Parser Integer
decimalInt = signed decimal <* string "\n"

-- TODO document the differences with Python's representation.
doubleFloat :: Parser Double
doubleFloat = double <* string "\n"

decimalLong :: Parser Integer
decimalLong = signed decimal <* string "L\n"

decodeLong1 :: Parser Integer
decodeLong1 = do
  n <- fromIntegral <$> anyWord8
  if n > 0
    then A.take n <&> byteStringLEToInteger
    else return 0

decodeLong4 :: Parser Integer
decodeLong4 = do
  n <- fromIntegral <$> anyInt32
  if n > 0
    then A.take n <&> byteStringLEToInteger
    else return 0

byteStringLEToInteger :: S.ByteString -> Integer
byteStringLEToInteger bs = if S.last bs > 127 then negate $ 256 ^ S.length bs - res else res
    where res = fst . fst $ toLong bs
          toLong :: S.ByteString -> ((Integer, Integer), S.ByteString)
          toLong = S.mapAccumL (\(a, b) w -> ((a + 256 ^ b * fromIntegral w, b + 1), w)) (0, 0)

stringnl :: Parser S.ByteString
stringnl = choice
  [ string "'" *> takeTill (== 39) <* string "'\n"
  ]

stringnl' :: Parser S.ByteString
stringnl' = takeTill (==10) <* string "\n"


float8 :: Parser Double
float8 = do
  w <- runGet getFloat64be <$> A.take 8
  case w of
    Left err -> fail err
    Right x -> return x

anyInt32 :: Parser Int32
anyInt32 = do
  w <- runGet getInt32le <$> A.take 4
  case w of
    Left err -> fail err
    Right x -> return x

anyInt64 :: Parser Int64
anyInt64 = do
  w <- runGet getInt64le <$> A.take 8
  case w of
    Left err -> fail err
    Right x -> return x


uint2 :: Parser Integer
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
  BINGET i -> putByteString "h" >> putWord8 (fromIntegral i)
  BINPUT i -> putByteString "q" >> putWord8 (fromIntegral i)
  BININT i -> putByteString "J" >> putWord32le (fromIntegral i)
  BININT1 i -> putByteString "K" >> putWord8 (fromIntegral i)
  BININT2 i -> putByteString "M" >> putUint2 i
  NONE -> putByteString "N"
  NEWTRUE -> putByteString "\136"
  NEWFALSE -> putByteString "\137"
  LONG1 0 -> putByteString "\138\NUL"
  LONG1 i -> putByteString "\138" >> encodeLong1 i
  BINFLOAT d -> putByteString "G" >> putFloat8 d
  SHORT_BINSTRING s -> do
    putByteString "U"
    putWord8 . fromIntegral $ S.length s
    putByteString s
  BINUNICODE s -> do
    putByteString "X"
    putWord32le . fromIntegral $ S.length s
    putByteString s
  EMPTY_DICT -> putByteString "}"
  EMPTY_LIST -> putByteString "]"
  EMPTY_TUPLE -> putByteString ")"
  TUPLE -> putByteString "t"
  TUPLE1 -> putByteString "\133"
  TUPLE2 -> putByteString "\134"
  TUPLE3 -> putByteString "\135"
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

putUint2 :: Integer -> Put
putUint2 d = putWord16le (fromIntegral d)

encodeLong1 :: Integer -> Put
encodeLong1 i = do
  -- TODO is it possible to know xs length without really constructing xs?
  let xs = f $ abs i
      f j | j < 256 = [fromIntegral j]
          | otherwise = let (n, r) = j `divMod` 256 in fromIntegral r : f n
      e = 256 ^ length xs
  if i < 0
    then do
      if abs i > e `div` 2
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
    INT Integer
  | BININT Integer
  | BININT1 Integer
  | BININT2 Integer
  | LONG Integer
  | LONG1 Integer
  | LONG4 Integer

  -- Strings
  | STRING S.ByteString
  | BINSTRING S.ByteString
  | SHORT_BINSTRING S.ByteString
  | BINBYTES S.ByteString
  | SHORT_BINBYTES S.ByteString
  | BINBYTES8 S.ByteString
  | BYTEARRAY8 S.ByteString

  -- Out-of-band buffer
  | NEXT_BUFFER
  | READONLY_BUFFER

  -- None
  | NONE

  -- Booleans
  | NEWTRUE
  | NEWFALSE

  -- Unicode strings
  | UNICODE T.Text
  | BINUNICODE S.ByteString
  | SHORT_BINUNICODE S.ByteString
  | BINUNICODE8 S.ByteString

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

  -- Sets
  | EMPTY_SET
  | ADDITEMS

  -- Frozen sets
  | FROZENSET

  -- Stack manipulation
  | POP
  | DUP
  | MARK
  | POP_MARK

  -- Memo manipulation
  | GET Int
  | BINGET Int
  | LONG_BINGET Integer
  | PUT Int
  | BINPUT Int
  | LONG_BINPUT Integer
  | MEMOIZE

  -- Extension registry (predefined objects)
  | EXT1 Int
  | EXT2 Int
  | EXT4 Int

  -- Various
  | GLOBAL S.ByteString S.ByteString
  | STACK_GLOBAL

  | REDUCE
  | BUILD
  | INST S.ByteString S.ByteString
  | OBJ
  | NEWOBJ
  | NEWOBJ_EX

  -- Pickle machine control
  | PROTO Int -- in [2..255]
  | STOP

  -- Persistent IDs
  | FRAME Int64
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
protocol3 = [BINBYTES, SHORT_BINBYTES]
protocol4 = [SHORT_BINUNICODE, BINUNICODE8, BINBYTES8, EMPTY_SET, ADDITEMS,
  FROZENSET, NEWOBJ_EX, STACK_GLOBAL, MEMOIZE, FRAME]
protocol5 = [BYTEARRAY8, NEXT_BUFFER, READONLY_BUFFER]
-}

----------------------------------------------------------------------
-- Pyhon value representation
----------------------------------------------------------------------

-- Maybe I can call them Py? And Have IsString/Num instances?
data Value =
    Dict [(Value,Value)]
  | List [Value]
  | Tuple [Value]
  | Set (SET.Set Value)
  | FrozenSet (SET.Set Value)
  | None
  | Bool Bool
  | BinInt Integer
  | BinLong Integer
  | BinFloat Double
  | BinString S.ByteString
  | BinUnicode S.ByteString
  | ClassObject S.ByteString S.ByteString
  | ObjectInstance
  | MarkObject -- Urk, not really a value.
  deriving (Eq, Ord, Show)

----------------------------------------------------------------------
-- Pickle machine (opcodes to value)
----------------------------------------------------------------------

unpickle' :: [OpCode] -> Either String Value
unpickle' xs = execute xs [] IM.empty

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
executeOne (INT i) stack memo = return (BinInt i:stack, memo)
executeOne (BININT i) stack memo = return (BinInt i:stack, memo)
executeOne (BININT1 i) stack memo = return (BinInt i:stack, memo)
executeOne (BININT2 i) stack memo = return (BinInt i:stack, memo)
executeOne (LONG i) stack memo = return (BinLong i:stack, memo)
executeOne (LONG1 i) stack memo = return (BinLong i:stack, memo)
executeOne (LONG4 i) stack memo = return (BinLong i:stack, memo)

executeOne (STRING s) stack memo = return (BinString s:stack, memo)
executeOne (BINSTRING s) stack memo = return (BinString s:stack, memo)
executeOne (SHORT_BINSTRING s) stack memo = return (BinString s:stack, memo)
executeOne (BINBYTES s) stack memo = return (BinString s:stack, memo)
executeOne (SHORT_BINBYTES s) stack memo = return (BinString s:stack, memo)
executeOne (BINBYTES8 s) stack memo = return (BinString s:stack, memo)
executeOne (BYTEARRAY8 s) stack memo = return (BinString s:stack, memo)

executeOne NONE stack memo = return (None:stack, memo)

executeOne NEWTRUE stack memo = return (Bool True:stack, memo)
executeOne NEWFALSE stack memo = return (Bool False:stack, memo)

executeOne (UNICODE b) stack memo = return (BinUnicode (encodeUtf8 b):stack, memo)
executeOne (SHORT_BINUNICODE b) stack memo = return (BinUnicode b:stack, memo)
executeOne (BINUNICODE b) stack memo = return (BinUnicode b:stack, memo)
executeOne (BINUNICODE8 b) stack memo = return (BinUnicode b:stack, memo)

executeOne (FLOAT d) stack memo = return (BinFloat d:stack, memo)
executeOne (BINFLOAT d) stack memo = return (BinFloat d:stack, memo)

executeOne EMPTY_LIST stack memo = return (List []: stack, memo)
executeOne APPEND stack memo = executeAppend stack memo
executeOne APPENDS stack memo = executeAppends [] stack memo
executeOne LIST stack memo = executeList [] stack memo

executeOne EMPTY_TUPLE stack memo = return (Tuple []: stack, memo)
executeOne TUPLE stack memo = executeTuple [] stack memo
executeOne TUPLE1 (a:stack) memo = return (Tuple [a]:stack, memo)
executeOne TUPLE2 (b:a:stack) memo = return (Tuple [a, b]:stack, memo)
executeOne TUPLE3 (c:b:a:stack) memo = return (Tuple [a, b, c]:stack, memo)

executeOne EMPTY_DICT stack memo = return (Dict []: stack, memo)
executeOne DICT stack memo = executeDict [] stack memo
executeOne SETITEM stack memo = executeSetitem stack memo
executeOne SETITEMS stack memo = executeSetitems [] stack memo

executeOne EMPTY_SET stack memo = return (Set SET.empty:stack, memo)
executeOne ADDITEMS stack memo = return (newStack', memo)
  where (items,newStack) = span (\case
                  (Set _) -> False
                  _       -> True) stack
        insertAll s = foldr SET.insert s items
        newStack' = (\case
          (Set s:rest) -> Set (insertAll s) : rest
          nonSet       -> nonSet) newStack

executeOne FROZENSET stack memo = executeFrozenSet SET.empty stack memo

executeOne POP (_:stack) memo = return (stack, memo)
executeOne DUP (x:stack) memo = return (x:x:stack, memo)
executeOne MARK stack memo = return (MarkObject:stack, memo)
executeOne POP_MARK (MarkObject:stack) memo = return (stack, memo)
executeOne POP_MARK (_:stack) memo = executeOne POP_MARK stack memo

executeOne (GET i) stack memo = executeLookup i stack memo
executeOne (BINGET i) stack memo = executeLookup i stack memo
executeOne (LONG_BINGET i) stack memo = executeLookup (fromInteger i) stack memo
executeOne (PUT i) (s:stack) memo = return (s:stack, IM.insert i s memo)
executeOne (BINPUT i) (s:stack) memo = return (s:stack, IM.insert i s memo)
executeOne (LONG_BINPUT i) (s:stack) memo = return (s:stack, IM.insert (fromInteger i) s memo)
executeOne MEMOIZE (s:stack) memo = return (s:stack, IM.insert (IM.size memo) s memo)

-- EXT codes are not implemented

executeOne (GLOBAL m c) stack memo = return ((ClassObject m c):stack, memo)
executeOne STACK_GLOBAL ((BinString c):(BinString m):stack) memo = return ((ClassObject m c):stack, memo)
executeOne STACK_GLOBAL ((BinUnicode c):(BinUnicode m):stack) memo = return ((ClassObject m c):stack, memo)

executeOne REDUCE ((Tuple t):(ClassObject moduleName className):stack) memo =
    case moduleName of
      "builtins" ->
          case className of
            "str" ->
                case t of
                  [BinString s] -> return ((BinString s):stack, memo)
                  [BinUnicode s] -> return ((BinUnicode s):stack, memo)
                  _ -> eOneErr ("Invalid input to str(): " ++ show t) stack memo
            _ -> eOneErr ("Builtin " ++ show className ++ " not supported.") stack memo
      _ -> eOneErr ("Class " ++ show moduleName ++ "." ++ show className ++ " not supported.") stack memo
-- This returns the original object without running __setstate__. Is it better to do this than fail?
executeOne BUILD (_:stack) memo = return (stack, memo)
executeOne (INST _ _) (MarkObject:stack) memo = return (ObjectInstance:stack, memo)
executeOne (INST m c) (_:stack) memo = executeOne (INST m c) stack memo
executeOne OBJ (MarkObject:stack) memo = return (ObjectInstance:stack, memo)
executeOne OBJ (_:stack) memo = executeOne OBJ stack memo
executeOne NEWOBJ (_:_:stack) memo = return (ObjectInstance:stack, memo)
executeOne NEWOBJ_EX (_:_:_:stack) memo = return (ObjectInstance:stack, memo)

executeOne (PROTO _) stack memo = return (stack, memo)
executeOne STOP stack memo = return (stack, memo)

executeOne (FRAME _) stack memo = return (stack, memo)
executeOne (PERSID _) stack memo = return (ObjectInstance:stack, memo)
executeOne BINPERSID (_:stack) memo = return (ObjectInstance:stack, memo)


executeOne op stack memo = eOneErr ("Can't execute opcode " ++ show op) stack memo

eOneErr :: String -> Stack -> Memo -> Either String (Stack, Memo)
eOneErr e stack memo = Left (e ++ ". stack: " ++ show stack ++ ", memo: " ++ show memo ++ ".")

executeLookup :: Int -> Stack -> Memo -> Either String (Stack, Memo)
executeLookup k stack memo = case IM.lookup k memo of
  Nothing -> Left "Unknown memo key"
  Just s -> Right (s:stack, memo)

executeTuple :: [Value] -> Stack -> Memo -> Either String ([Value], Memo)
executeTuple l (MarkObject:stack) memo = return (Tuple l:stack, memo)
executeTuple l (a:stack) memo = executeTuple (a : l) stack memo
executeTuple _ _ _ = Left "Empty stack in executeTuple"

executeDict :: [(Value, Value)] -> Stack -> Memo -> Either String ([Value], Memo)
executeDict l (MarkObject:stack) memo = return (l `addToDict` Dict []:stack, memo)
executeDict l (a:b:stack) memo = executeDict ((b, a) : l) stack memo
executeDict _ _ _ = Left "Empty stack in executeDict"

executeList :: [Value] -> Stack -> Memo -> Either String ([Value], Memo)
executeList l (MarkObject:stack) memo = return (List l:stack, memo)
executeList l (x:stack) memo = executeList (x : l) stack memo
executeList _ _ _ = Left "Empty stack in executeList"

executeSetitem :: Stack -> Memo -> Either String ([Value], Memo)
executeSetitem (v:k:Dict d:stack) memo = return (Dict (d ++ [(k,v)]):stack, memo)
executeSetitem _ _ = Left "Empty stack in executeSetitem"

executeSetitems :: [(Value, Value)] -> Stack -> Memo -> Either String ([Value], Memo)
executeSetitems l (MarkObject:Dict d:stack) memo = return (reverse l `addToDict` Dict d:stack, memo)
executeSetitems l (a:b:stack) memo = executeSetitems ((b, a) : l) stack memo
executeSetitems _ _ _ = Left "Empty stack in executeSetitems"

executeAppend :: Stack -> Memo -> Either String ([Value], Memo)
executeAppend (x:List xs:stack) memo = return (List (xs ++ [x]):stack, memo)
executeAppend _ _ = Left "Empty stack in executeAppend"

executeAppends :: [Value] -> Stack -> Memo -> Either String ([Value], Memo)
executeAppends l (MarkObject:List xs:stack) memo = return (List (xs ++ l):stack, memo)
executeAppends l (x:stack) memo = executeAppends (x : l) stack memo
executeAppends _ _ _ = Left "Empty stack in executeAppends"

executeFrozenSet :: SET.Set Value -> Stack -> Memo -> Either String ([Value], Memo)
executeFrozenSet s (MarkObject:stack) memo = return (FrozenSet s:stack, memo)
executeFrozenSet s (x:stack) memo = executeFrozenSet (SET.insert x s) stack memo
executeFrozenSet _ _ _ = Left "Empty frozen set in executeFrozenSet"


addToDict :: [(Value, Value)] -> Value -> Value
addToDict l (Dict d) = Dict $ foldl' add d l
  where add d' (k, v) = (k,v):d'
addToDict _ _ = error "Second argument to addToDict must be a dict"

----------------------------------------------------------------------
-- Pickling (value to opcodes)
----------------------------------------------------------------------

newtype Pickler a = Pickler { runP :: WriterT [OpCode] (State (Map Value Int)) a }
  deriving (Functor, Applicative, Monad, MonadWriter [OpCode], MonadState (Map Value Int))

runPickler :: Pickler () -> [OpCode]
runPickler p = evalState (execWriterT (runP p)) M.empty

pickle' :: Value -> Pickler ()
pickle' value = do
  m <- get
  case M.lookup value m of
    Just k -> tell [BINGET k]
    Nothing -> case value of
      Dict d -> pickleDict d
      List xs -> pickleList xs
      Tuple xs -> pickleTuple xs
      None -> tell [NONE]
      Bool True -> tell [NEWTRUE]
      Bool False -> tell [NEWFALSE]
      BinInt i -> pickleBinInt i
      BinLong i -> pickleBinLong i
      BinFloat d -> pickleBinFloat d
      BinString s -> pickleBinString s
      BinUnicode s -> pickleBinUnicode s
      x -> error $ "TODO: pickle " ++ show x

-- TODO actually lookup values in the map, reusing their key.
binput' :: Value -> Pickler ()
binput' value = do
  i <- gets M.size
  m <- get
  put (M.insert value i m)
  tell [BINPUT i]

pickleDict :: [(Value,Value)] -> Pickler ()
pickleDict d = do
  tell [EMPTY_DICT]
  binput' (Dict d)

  case d of
    [] -> return ()
    [(k,v)] -> pickle' k >> pickle' v >> tell [SETITEM]
    _ -> do
      tell [MARK]
      mapM_ (\(k, v) -> pickle' k >> pickle' v) d
      tell [SETITEMS]

pickleList :: [Value] -> Pickler ()
pickleList xs = do
  tell [EMPTY_LIST]
  binput' (List xs)

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
pickleTuple [a, b, c] = do
  pickle' a
  pickle' b
  pickle' c
  tell [TUPLE3]
  binput' (Tuple [a, b, c])
pickleTuple xs = do
  tell [MARK]
  mapM_ pickle' xs
  tell [TUPLE]
  binput' (Tuple xs)

pickleBinInt :: Integer -> Pickler ()
pickleBinInt i | i >= 0 && i < 256 = tell [BININT1 i]
               | i >= 256 && i < 65536 = tell [BININT2 i]
               | otherwise = tell [BININT i]

pickleBinLong :: Integer -> Pickler ()
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

pickleBinUnicode :: S.ByteString -> Pickler ()
pickleBinUnicode s = do
  tell [BINUNICODE s]
  binput' (BinUnicode s)

----------------------------------------------------------------------
-- Manipulate Values
----------------------------------------------------------------------

dictGet :: Value -> Value -> Either String (Maybe Value)
dictGet (Dict d) v = return $ M.lookup v (M.fromList d)
dictGet _ _ = Left "dictGet: not a dict."

dictGet' :: Value -> Value -> Either String Value
dictGet' (Dict d) v = case M.lookup v (M.fromList d) of
  Just value -> return value
  Nothing -> Left "dictGet': no such key."
dictGet' _ _ = Left "dictGet': not a dict."

dictGetString :: Value -> S.ByteString -> Either String S.ByteString
dictGetString (Dict d) s = case M.lookup (BinString s) (M.fromList d) of
  Just (BinString s') -> return s'
  _ -> Left "dictGetString: not a dict, or no such key."
dictGetString v _ = error ("Can only run dictGetString on a Dict, you run it on " ++ show v ++ ".")

----------------------------------------------------------------------
-- Convert Values to Haskell Representation
----------------------------------------------------------------------

class FromValue a where
    fromVal :: Value -> Maybe a

{-
data Value =
    Dict (Map Value Value)
  | List [Value]
  | Tuple [Value]
  | Set (SET.Set Value)
  | None
  | Bool Bool
  | BinInt Integer
  | BinLong Integer
  | BinFloat Double
  | BinString S.ByteString
  | BinUnicode S.ByteString
  | MarkObject -- Urk, not really a value.
  deriving (Eq, Ord, Show)
-}

instance FromValue Int where
    fromVal (BinInt i) = Just (fromInteger i)
    fromVal (BinLong i) = Just (fromInteger i)
    fromVal _          = Nothing

instance FromValue Integer where
    fromVal (BinInt i) = Just i
    fromVal (BinLong i) = Just i
    fromVal _          = Nothing

instance FromValue S.ByteString where
    fromVal (BinString s) = Just s
    fromVal (BinUnicode s) = Just s
    fromVal _             = Nothing

instance FromValue Double where
    fromVal (BinFloat d) = Just d
    fromVal _            = Nothing

instance FromValue T.Text where
    fromVal (BinString bs) = Just (T.decodeUtf8 bs)
    fromVal (BinUnicode bs) = Just (T.decodeUtf8 bs)
    fromVal _              = Nothing

instance FromValue Bool where
    fromVal (Bool b) = Just b
    fromVal _        = Nothing

instance (FromValue a) => FromValue [a] where
    fromVal (List as)  = mapM fromVal as
    fromVal (Tuple as) = mapM fromVal as
    fromVal (Set as)   = mapM fromVal (SET.toList as)
    fromVal _          = Nothing

instance (FromValue a, Ord a) => FromValue (SET.Set a) where
    fromVal (Set as) = SET.fromList <$> fromVal (Set as)
    fromVal _        = Nothing

instance (FromValue k, FromValue v, Ord k) => FromValue (Map k v) where
    fromVal (Dict m) =
        M.foldrWithKey
            (\k v maybeM -> M.insert <$> fromVal k <*> fromVal v <*> maybeM)
            (Just M.empty) (M.fromList m)
    fromVal _        = Nothing

instance (FromValue a, FromValue b) =>
    FromValue (a, b) where
    fromVal (Tuple [a,b]) = (,) <$> fromVal a <*> fromVal b
    fromVal (List [a,b])  = (,) <$> fromVal a <*> fromVal b
    fromVal _             = Nothing

instance (FromValue a, FromValue b, FromValue c) =>
    FromValue (a, b, c) where
    fromVal (Tuple [a,b,c]) = (,,) <$> fromVal a <*> fromVal b <*> fromVal c
    fromVal (List [a,b,c])  = (,,) <$> fromVal a <*> fromVal b <*> fromVal c
    fromVal _             = Nothing

instance (FromValue a, FromValue b, FromValue c, FromValue d) =>
    FromValue (a, b, c, d) where
    fromVal (Tuple [a,b,c,d]) = (,,,) <$> fromVal a <*> fromVal b <*> fromVal c <*> fromVal d
    fromVal (List [a,b,c,d])  = (,,,) <$> fromVal a <*> fromVal b <*> fromVal c <*> fromVal d
    fromVal _             = Nothing

instance (FromValue a, FromValue b, FromValue c, FromValue d, FromValue e) =>
    FromValue (a, b, c, d, e) where
    fromVal (Tuple [a,b,c,d,e]) = (,,,,) <$> fromVal a <*> fromVal b <*> fromVal c <*> fromVal d <*> fromVal e
    fromVal (List [a,b,c,d,e])  = (,,,,) <$> fromVal a <*> fromVal b <*> fromVal c <*> fromVal d <*> fromVal e
    fromVal _             = Nothing

instance (FromValue a, FromValue b, FromValue c, FromValue d, FromValue e, FromValue f) =>
    FromValue (a, b, c, d, e, f) where
    fromVal (Tuple [a,b,c,d,e,f]) = (,,,,,) <$> fromVal a <*> fromVal b <*> fromVal c <*> fromVal d <*> fromVal e <*> fromVal f
    fromVal (List [a,b,c,d,e,f])  = (,,,,,) <$> fromVal a <*> fromVal b <*> fromVal c <*> fromVal d <*> fromVal e <*> fromVal f
    fromVal _             = Nothing

instance (FromValue a, FromValue b, FromValue c, FromValue d, FromValue e, FromValue f, FromValue g) =>
    FromValue (a, b, c, d, e, f, g) where
    fromVal (Tuple [a,b,c,d,e,f,g]) = (,,,,,,) <$> fromVal a <*> fromVal b <*> fromVal c <*> fromVal d <*> fromVal e <*> fromVal f <*> fromVal g
    fromVal (List [a,b,c,d,e,f,g])  = (,,,,,,) <$> fromVal a <*> fromVal b <*> fromVal c <*> fromVal d <*> fromVal e <*> fromVal f <*> fromVal g
    fromVal _             = Nothing
