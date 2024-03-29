{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This test suite for the `python-pickle` package uses Python to dump
-- various pickled objects to a temporary file, and for each dump checks
-- that `Language.Python.Pickle` can unpickle the object, and repickle it
-- exactly as the original dump.
module Main (main) where

import Control.Arrow ((&&&))
import Control.Monad (when)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import System.Directory (removeFile)
import System.Process (rawSystem)
import Test.HUnit (assertEqual, assertFailure)
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Language.Python.Pickle

main :: IO ()

main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "PickledValues protocol 0" $
      map (\(a, b) -> testCase a $ testAgainstPython 0 b a) expressions
  , testGroup "PickledValues protocol 1" $
      map (\(a, b) -> testCase a $ testAgainstPython 1 b a) expressions
  , testGroup "PickledValues protocol 2" $
      map (\(a, b) -> testCase a $ testAgainstPython 2 b a) expressions
  , testGroup "PickledValues protocol 3" $
      map (\(a, b) -> testCase a $ testAgainstPython 2 b a) expressions
  , testGroup "PickledValues protocol 4" $
      map (\(a, b) -> testCase a $ testAgainstPython 2 b a) expressions
  , testGroup "PickledValues protocol 5" $
      map (\(a, b) -> testCase a $ testAgainstPython 2 b a) expressions
  ]

-- The round-tripping (unpickling/pickling and comparing the original
-- with the result) is not enough. For instance incorrectly casting ints to
-- ints can be hidden (the unpickled value is incorrect but when pickled again, it
-- is the same as the original).
-- So we can provide an expected value.
testAgainstPython :: Int -> Value -> String -> IO ()
testAgainstPython protocol expected s = do
  let filename = "python-pickle-test-pickled-values.pickle"
  _ <- rawSystem "./tests/pickle-dump.py" ["--output", filename,
    "--protocol", show protocol, "--", s]
  content <- S.readFile filename
  let value = unpickle content

  case value of
    Left err -> assertFailure $ "Can't unpickle " ++ s
      ++ show content ++ ".\nUnpickling error:\n  " ++ err
    Right v -> do
      assertEqual "Unpickled value against expected value" expected v
      when (protocol == 2) $
        assertEqual "Pickled value against original file content"
          content (pickle v)
  removeFile filename

expressions :: [(String, Value)]
expressions =
  [ ("{}", Dict [])
  , ("{'type': 'cache-query'}",
      Dict [(BinUnicode "type", BinUnicode "cache-query")])
  , ("{'type': 'cache-query', 'metric': 'some.metric.name'}",
      Dict [(BinUnicode "type", BinUnicode "cache-query"),
        (BinUnicode "metric", BinUnicode "some.metric.name")])

  , ("[]", List [])
  , ("[1]", List [BinInt 1])
  , ("[1, 2]", List [BinInt 1, BinInt 2])

  , ("()", Tuple [])
  , ("(1,)", Tuple [BinInt 1])
  , ("(1, 2)", Tuple [BinInt 1, BinInt 2])
  , ("(1, 2, 3)", Tuple [BinInt 1, BinInt 2, BinInt 3])
  , ("(1, 2, 3, 4)", Tuple [BinInt 1, BinInt 2, BinInt 3, BinInt 4])
  , ("((), [], [3, 4], {})",
    Tuple [Tuple [], List [], List [BinInt 3, BinInt 4], Dict []])

  , ("None", None)
  , ("True", Bool True)
  , ("False", Bool False)

  , ("{'datapoints': [(1, 2)]}",
      Dict [(BinUnicode "datapoints",
        List [Tuple [BinInt 1, BinInt 2]])])
  , ("{'datapoints': [(1, 2)], (2,): 'foo'}",
        Dict
          [ ( BinUnicode "datapoints"
            , List [Tuple [BinInt 1, BinInt 2]])
          , ( Tuple [BinInt 2]
            , BinUnicode "foo")
          ])
  , ("[(1, 2)]", List [Tuple [BinInt 1, BinInt 2]])
  , ("('twice', 'twice')", Tuple [BinUnicode "twice", BinUnicode "twice"])
  , ("{'pi': 3.14159}", Dict [(BinUnicode "pi", BinFloat 3.14159)])
  ]
  ++ map (show &&& BinInt) ints
  ++ map (show &&& BinFloat) doubles
  ++ map (quote . C.unpack &&& BinUnicode) strings
  -- ++ map ((++ "L") . show &&& BinLong) ints

ints :: [Integer]
ints =
  [0, 10..100] ++ [100, 150..1000] ++
  [1000, 1500..10000] ++ [10000, 50000..1000000] ++
  map negate ([1,126,127,128,129] ++ [10000, 50000..1000000])

doubles :: [Double]
doubles =
  [0.1, 10.1..100] ++
  map negate [0.1, 10.1..100]

strings :: [C.ByteString]
strings =
  ["cache-query"]

quote :: String -> String
quote s = concat ["'", s, "'"]
