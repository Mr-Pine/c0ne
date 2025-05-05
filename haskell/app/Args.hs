module Args
  ( jobParser
  , validateJob
  ) where

import Compile (Job(..), src)
import Error (L1ExceptT, generalFail)
import System.Directory (doesFileExist)

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Options.Applicative

jobP :: Parser Job
jobP =
  Job
    <$> argument str (metavar "INPUT" <> help "Input file to process")
    <*> argument str (metavar "OUTPUT" <> help "Name for the output file")

jobParser :: ParserInfo Job
jobParser =
  info
    (jobP <**> helper)
    (fullDesc
       <> progDesc "Compile L1 programs to a simple abstract assembly language"
       <> header "An simple starter compiler for the L1 language")

validateJob :: Job -> L1ExceptT Job
validateJob job = do
  let sourceFile = src job
  exists <- liftIO $ doesFileExist sourceFile
  unless exists $ generalFail ("File " ++ sourceFile ++ " does not exist :(") 1
  return job
