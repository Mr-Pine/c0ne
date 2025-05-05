module Compile
  ( Job(..)
  , compile
  ) where

import Compile.AAsm (codeGen)
import Compile.Parser (parseAST)
import Compile.Semantic (semanticAnalysis)
import Error (L1ExceptT)

import Control.Monad.IO.Class

data Job = Job
  { src :: FilePath
  , out :: FilePath
  } deriving (Show)

compile :: Job -> L1ExceptT ()
compile job = do
  ast <- parseAST $ src job
  semanticAnalysis ast
  let code = codeGen ast
  liftIO $ writeFile (out job) (unlines code)
  return ()
