module Main (main) where

import qualified Data.ByteString as S
import System.Environment (getArgs)

import Language.Python.Pickle

main :: IO ()
main = do
  [filename] <- getArgs
  content <- S.readFile filename
  putStrLn $ "File content:     " ++ show content
  let value = unpickle content
  case value of
    Left err -> putStrLn $ "Unpickling error: " ++ err
    Right v -> do
      putStrLn $ "After unpickling: " ++ show v
      putStrLn $ "After pickling:   " ++ show (pickle v)
