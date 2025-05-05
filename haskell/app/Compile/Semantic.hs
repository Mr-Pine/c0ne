module Compile.Semantic
  ( semanticAnalysis
  ) where

import           Compile.AST (AST(..), Expr(..), Stmt(..), posPretty)
import           Error (L1ExceptT, semanticFail)

import           Control.Monad (unless, when)
import           Control.Monad.State
import qualified Data.Map as Map

data VariableStatus
  = Declared
  | Initialized
  deriving (Show)

-- You might want to keep track of some location information as well at some point
type Namespace = Map.Map String VariableStatus

type L1Semantic = StateT Namespace L1ExceptT

-- A little wrapper so we don't have to ($ lift) everywhere inside the StateT
semanticFail' :: String -> L1Semantic a
semanticFail' = lift . semanticFail

semanticAnalysis :: AST -> L1ExceptT ()
semanticAnalysis ast = do
  ns <- varStatusAnalysis ast
  evalStateT (checkReturns ast) ns

-- right now an AST is just a list of statements
varStatusAnalysis :: AST -> L1ExceptT Namespace
varStatusAnalysis (Block stmts _) = do
  execStateT (mapM_ checkStmt stmts) Map.empty

maxInt :: Integer
maxInt = 2 ^ (31 :: Integer)

-- So far this checks:
-- + we cannot declare a variable again that has already been declared or initialized
-- + we cannot initialize a variable again that has already been declared or initialized
-- + a variable needs to be declared or initialized before we can assign to it
-- + we can only return valid expressions
checkStmt :: Stmt -> L1Semantic ()
checkStmt (Decl name pos) = do
  ns <- get
  let isDeclared = Map.member name ns
  when isDeclared
    $ semanticFail'
    $ "Variable " ++ name ++ " redeclared at: " ++ posPretty pos
  put $ Map.insert name Declared ns
checkStmt (Init name e pos) = do
  ns <- get
  let isDeclared = Map.member name ns
  when isDeclared
    $ semanticFail'
    $ "Variable " ++ name ++ " redeclared (initialized) at: " ++ posPretty pos
  checkExpr e
  put $ Map.insert name Initialized ns
checkStmt (Asgn name _ e pos) = do
  ns <- get
  unless (Map.member name ns)
    $ semanticFail'
    $ "Trying to assign to undeclared variable "
        ++ name
        ++ " at: "
        ++ posPretty pos
  checkExpr e
  put $ Map.insert name Initialized ns
checkStmt (Ret e _) = checkExpr e

checkExpr :: Expr -> L1Semantic ()
checkExpr (IntExpr n pos) = do
  when (n < 0 || n > maxInt)
    $ semanticFail'
    $ "Integer literal " ++ show n ++ " out of bounds at: " ++ posPretty pos
checkExpr (Ident name pos) = do
  ns <- get
  case Map.lookup name ns of
    Just Initialized -> return ()
    _ ->
      semanticFail'
        $ "Variable "
            ++ name
            ++ " used without initialization at: "
            ++ posPretty pos
checkExpr (UnExpr _ e) = checkExpr e
checkExpr (BinExpr _ lhs rhs) = checkExpr lhs >> checkExpr rhs

checkReturns :: AST -> L1Semantic ()
checkReturns (Block stmts _) = do
  let returns = any isReturn stmts
  unless returns $ semanticFail' "Program does not return"
  where
    isReturn (Ret _ _) = True
    isReturn _ = False
