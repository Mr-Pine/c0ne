module Error
  ( L1ExceptT
  , generalFail
  , parserFail
  , semanticFail
  , dieWithError
  ) where

import           Control.Monad.Except (ExceptT, throwError)
import qualified System.Exit as Exit
import           System.IO (hPutStrLn, stderr)

-- Predefined exit codes signaling compiler status
parserErrorCode :: Int
parserErrorCode = 42

semanticErrorCode :: Int
semanticErrorCode = 7

-- Error message and exit code
data L1Error
  = Error String Int
  | ParserError String
  | SemanticError String
  deriving (Show)

type L1ExceptT = ExceptT L1Error IO

-- Convenienve functions to throw exceptions
generalFail :: String -> Int -> L1ExceptT a
generalFail msg code = throwError $ Error msg code

parserFail :: String -> L1ExceptT a
parserFail = throwError . ParserError

semanticFail :: String -> L1ExceptT a
semanticFail = throwError . SemanticError

-- Exit with an error message and a return code
dieWithError :: L1Error -> IO ()
dieWithError (Error msg code) = do
  hPutStrLn stderr msg
  Exit.exitWith $ Exit.ExitFailure code
dieWithError (ParserError msg) = dieWithError (Error msg parserErrorCode)
dieWithError (SemanticError msg) = dieWithError (Error msg semanticErrorCode)
