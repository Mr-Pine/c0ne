module Compile
  ( Job(..)
  , compile
  ) where

import Compile.Parser (parseAST)
import Compile.Semantic (semanticAnalysis)
import Error (L1ExceptT)

import Control.Monad.IO.Class
import Compile.IR.SSA (ssaTransform)
import Text.Pretty.Simple (pPrint)
import Compile.Backend.Schedule (schedule)
import Compile.Backend.X86.Codegen (codeGen)

data Job = Job
  { src :: FilePath
  , out :: FilePath
  } deriving (Show)

compile :: Job -> L1ExceptT ()
compile job = do
  ast <- parseAST $ src job
  semanticAnalysis ast
  let ssa = ssaTransform "main" ast
  liftIO $ pPrint ssa
  let ssaSchedule = schedule ssa
  liftIO $ pPrint ssaSchedule
  let code = codeGen ssaSchedule
  liftIO $ writeFile (out job) (unlines code)
  return ()
