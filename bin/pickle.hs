{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Monad (when)
import Data.Version (showVersion)
import qualified Data.ByteString as S
import System.Console.CmdArgs.Implicit

import Paths_python_pickle (version)

import Language.Python.Pickle

main :: IO ()
main = (runCmd =<<) . cmdArgs $ modes
  [ cmdRead
  ]
  &= summary versionString
  &= program "pickle"

-- | String with the program name, version and copyright.
versionString :: String
versionString =
  "Pickle " ++ showVersion version ++ ". Copyright (c) 2012 Vo Minh Thu."

-- | Data type representing the different command-line subcommands.
data Cmd =
    Read
    { cmdFilename :: FilePath
    , cmdContent :: Bool
    , cmdUnpickle :: Bool
    , cmdRepickle :: Bool
    }
    -- ^ Parse a pickle file and display some information.
  deriving (Data, Typeable)

-- | Create a 'Read' command.
cmdRead :: Cmd
cmdRead = Read
  { cmdFilename = def
    &= argPos 0
    &= typ "FILE"
  , cmdContent = def
    &= explicit
    &= name "content"
    &= help "Display the file content `as-is`."
  , cmdUnpickle = def
    &= explicit
    &= name "unpickle"
    &= help "Display the file content as a Python object."
  , cmdRepickle = def
    &= explicit
    &= name "repickle"
    &= help "Display the unpickled Python object re-pickled."
  } &= help "Parse a pickle file and display some information."
    &= explicit
    &= name "read"

-- | Run a sub-command.
runCmd :: Cmd -> IO ()
runCmd Read{..} = do
  content <- S.readFile cmdFilename
  when cmdContent $ putStrLn $ "File content:       " ++ show content
  when (cmdUnpickle || cmdRepickle) $ do
    case unpickle content of
      Left err -> putStrLn $ "Unpickling error:   " ++ err
      Right v -> do
        when cmdUnpickle $ putStrLn $ "Unpickled object:   " ++ show v
        when cmdRepickle $ putStrLn $ "Re-pickled content: " ++ show (pickle v)
