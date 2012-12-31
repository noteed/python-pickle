{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Very partial implementation of the Python Pickle Virtual Machine
-- (protocol 2): i.e. parses pickled data into opcodes, then executes the
-- opcodes to construct a (Haskell representation of a) Python object.
module Language.Python.Pickle where

import Control.Applicative ((<$>), (*>))
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.ByteString as S
import Data.Attoparsec hiding (take)
import qualified Data.Attoparsec as A
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

-- Note \128 is the same as \STX\x80\02, i.e. protocol 2.

unpickle :: S.ByteString -> Either String Value
unpickle s = do
  xs <- parseOnly (string "\128\STX" *> many1 opcodes) s
  unpickle' xs

pickle :: Value -> S.ByteString
pickle value = runPut $ do
  putByteString "\128\STX"
  mapM_ serialize . runPickler $ pickle' value
  putByteString "."

----------------------------------------------------------------------
-- Pickle opcodes parser
----------------------------------------------------------------------

-- Maybe parsing could be done with a big switch and only cereal,
-- instead of relying on attoparsec's "choice" combinator.

opcodes :: Parser OpCode
empty_dict, empty_list, empty_tuple, tuple1, tuple2 :: Parser OpCode
binput, mark :: Parser OpCode
binint, binint1, binint2, binfloat :: Parser OpCode
short_binstring :: Parser OpCode
setitem, setitems :: Parser OpCode
append, appends :: Parser OpCode
stop :: Parser OpCode

opcodes = choice
  [ empty_dict, empty_list, empty_tuple, tuple1, tuple2
  , binput, mark
  , binint, binint1, binint2, binfloat
  , short_binstring
  , setitem, setitems
  , append, appends
  , stop
  ]

empty_dict = string "}" *> return EMPTY_DICT

empty_list = string "]" *> return EMPTY_LIST

empty_tuple = string ")" *> return EMPTY_TUPLE

binput = string "q" *> (BINPUT . fromIntegral <$> anyWord8)

mark = string "(" *> return MARK

binint = string "J" *> (BININT <$> int4)

binint1 = string "K" *> (BININT1 . fromIntegral <$> anyWord8)

binint2 = string "M" *> (BININT2 <$> uint2)

binfloat = string "G" *> (BINFLOAT <$> float8)

short_binstring = do
  _ <- string "U"
  i <- fromIntegral <$> anyWord8
  s <- A.take i
  return $ SHORT_BINSTRING s

tuple1 = string "\133" *> return TUPLE1 -- same as \x85

tuple2 = string "\134" *> return TUPLE2 -- same as \x86

setitem = string "s" *> return SETITEM

setitems = string "u" *> return SETITEMS

append = string "a" *> return APPEND

appends = string "e" *> return APPENDS

stop = string "." *> return STOP

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

----------------------------------------------------------------------
-- Pickle opcodes
----------------------------------------------------------------------

data OpCode =
    EMPTY_DICT
  | EMPTY_LIST
  | EMPTY_TUPLE
  | BINPUT Int
  | MARK
  | BININT Int
  | BININT1 Int
  | BININT2 Int
  | BINFLOAT Double
  | SHORT_BINSTRING S.ByteString
  | TUPLE1
  | TUPLE2
  | SETITEM
  | SETITEMS
  | APPEND
  | APPENDS
  | STOP
  deriving Show

----------------------------------------------------------------------
-- Pyhon value representation
----------------------------------------------------------------------

-- Maybe I can call them Py? And Have IsString/Num instances?
data Value =
    Dict (Map Value Value)
  | List [Value]
  | Tuple [Value]
  | BinInt Int
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
executeOne (BINPUT i) (s:stack) memo = return (s:stack, IM.insert i s memo)
executeOne (BININT i) stack memo = return (BinInt i:stack, memo)
executeOne (BININT1 i) stack memo = return (BinInt i:stack, memo)
executeOne (BININT2 i) stack memo = return (BinInt i:stack, memo)
executeOne (BINFLOAT d) stack memo = return (BinFloat d:stack, memo)
executeOne (SHORT_BINSTRING s) stack memo = return (BinString s:stack, memo)
executeOne MARK stack memo = return (MarkObject:stack, memo)
executeOne TUPLE1 (a:stack) memo = return (Tuple [a]:stack, memo)
executeOne TUPLE2 (b:a:stack) memo = return (Tuple [a, b]:stack, memo)
executeOne SETITEM stack memo = executeSetitem stack memo
executeOne SETITEMS stack memo = executeSetitems [] stack memo
executeOne APPEND stack memo = executeAppend stack memo
executeOne APPENDS stack memo = executeAppends [] stack memo
executeOne STOP stack memo = Right (stack, memo)
executeOne _ _ _ = Left "`executeOne` unimplemented"

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

-- TODO depending on the int range, it should not always be a BININT1
pickleBinInt :: Int -> Pickler ()
pickleBinInt i | i >= 0 && i < 256 = tell [BININT1 i]
               | i >= 256 && i < 65536 = tell [BININT2 i]
               | otherwise = tell [BININT i]

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
