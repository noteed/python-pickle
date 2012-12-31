{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Arrow ((&&&))
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import System.Process (rawSystem)

import Data.Pickle

main :: IO ()
main = mapM_ (\(a, b) -> testAgainstPython (Just b) a) expressions

-- The round-tripping (unpickling/pickling and comparing the original
-- with the result) is not enough. For instance incorrectly casting ints to
-- ints can be hidden (the unpickled value is incorrect but when pickled again, it
-- is the same as the original).
-- So we can provide an expected value.
testAgainstPython mexpected s = do
  let filename = "test-pickle.pickle"
  rawSystem "./tests/pickle-dump.py" [s, "--output", filename]
  content <- S.readFile filename
  let value = unpickle content

  case value of
    Left err -> putStrLn $ "Failure:\n  Can't unpickle " ++ s ++ "\n  "
      ++ show content ++ "\n  unpickling error: " ++ err
    Right v | pickle v == content -> do
      case mexpected of
        Nothing -> putStrLn $ "Success: " ++ s
        Just expected | expected == v -> putStrLn $ "Success: " ++ s
        Just expected -> putStrLn $ "Failure: unexpected unpickled value:\n  "
          ++ show v ++ "\n  " ++ show expected
    Right v -> putStrLn $
      "Failure:\n  Pickled value differ from the orignal:\n  "
      ++ show content ++ "\n  " ++ show (pickle v)

expressions =
  [ ("{}", Dict M.empty)
  , ("{'type': 'cache-query'}",
      Dict $ M.fromList [(BinString "type", BinString "cache-query")])
  , ("{'type': 'cache-query', 'metric': 'some.metric.name'}",
      Dict $ M.fromList [(BinString "type", BinString "cache-query"),
        (BinString "metric", BinString "some.metric.name")])

  , ("[]", List [])
  , ("[1]", List [BinInt 1])
  , ("[1, 2]", List [BinInt 1, BinInt 2])

  , ("()", Tuple [])
  , ("(1,)", Tuple [BinInt 1])
  , ("(1, 2)", Tuple [BinInt 1, BinInt 2])

  , ("{'datapoints': [(1, 2)]}",
      Dict $ M.fromList [(BinString "datapoints",
        List [Tuple [BinInt 1, BinInt 2]])])
  , ("{'datapoints': [(1, 2)], (2,): 'foo'}",
        Dict $ M.fromList
          [ ( BinString "datapoints"
            , List [Tuple [BinInt 1, BinInt 2]])
          , ( Tuple [BinInt 2]
            , BinString "foo")
          ])
  , ("[(1, 2)]", List [Tuple [BinInt 1, BinInt 2]])
  ]
  ++ map (show &&& BinInt) ints
  ++ map (show &&& BinFloat) doubles
  ++ map (quote . C.unpack &&& BinString) strings

ints =
  [0, 10..100] ++ [100, 150..1000] ++
  [1000, 1500..10000] ++ [10000, 50000..1000000] ++
  map negate [10000, 50000..1000000]

doubles =
  [0.1, 10.1..100] ++
  map negate [0.1, 10.1..100]

strings =
  ["cache-query"]

quote s = concat ["'", s, "'"]
