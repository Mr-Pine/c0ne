{-# LANGUAGE OverloadedRecordDot #-}

module Compile
  ( Job(..)
  , compile
  ) where

import Compile.Parser (parseAST)
import Compile.Semantic (semanticAnalysis)
import Error (L1ExceptT, generalFail)

import Control.Monad.IO.Class
import Compile.IR.SSA (ssaTransform)
import Text.Pretty.Simple (pPrint)
import Compile.Backend.Schedule (schedule)
import Compile.Backend.X86.Codegen (codeGen)
import GHC.IO.Handle (hPutStr, Handle, hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (CreateProcess(std_err), createProcess, waitForProcess, StdStream (Inherit), proc)
import System.Exit (ExitCode (ExitSuccess))
import Control.Monad (unless)

data Job = Job
  { src :: FilePath
  , out :: FilePath
  } deriving (Show)

compile :: Job -> L1ExceptT ()
compile job = do
  ast <- parseAST $ src job
  semanticAnalysis ast
  let ssa = ssaTransform "main" ast
  let ssaSchedule = schedule ssa
  let code = codeGen ssaSchedule
  liftIO $ putStrLn . unlines $ code
  gccExitCode <- liftIO $ withSystemTempFile "c0ne.S" (assemble (unlines code) job.out)
  unless (gccExitCode == ExitSuccess) $ generalFail "C0ne too spikey for gcc :(" 1

assemble :: String -> FilePath -> FilePath -> Handle -> IO ExitCode
assemble code out tmp_path handle = do
    hPutStr handle code
    hClose handle
    (_, _, _, procHandle) <- createProcess (proc "gcc" ["-Wl,-e_main", tmp_path, "-o", out]){std_err = Inherit}
    waitForProcess procHandle
    
