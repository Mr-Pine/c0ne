module Compile.AST
  ( AST(..)
  , Stmt(..)
  , Expr(..)
  , IntLiteral(..)
  , Op(..)
  , Identifier
  , showAsgnOp
  , posPretty
  ) where

import Data.List (intercalate)
import Text.Megaparsec
import Numeric (showHex)

data AST =
  Block [Stmt] SourcePos

type Identifier = String

data Stmt
  = Decl Identifier SourcePos
  | Init Identifier Expr SourcePos
  | Asgn Identifier AsgnOp Expr SourcePos
  | Ret Expr SourcePos

data Expr
  = IntExpr IntLiteral SourcePos
  | Ident Identifier SourcePos
  | UnExpr Op Expr
  | BinExpr Op Expr Expr

data IntLiteral = DecLit { value :: Integer } | HexLit { value :: Integer }

-- Nothing means =, Just is for +=, %=, ...
type AsgnOp = Maybe Op

data Op
  = Mul
  | Add
  | Sub
  | Div
  | Neg
  | Mod
  | Nop
  deriving (Eq)

-- re-exported for convenience
posPretty :: SourcePos -> String
posPretty = sourcePosPretty

-- Some very basic pretty printing
instance Show AST where
  show (Block stmts _) =
    "Block: {\n" ++ intercalate "\n" (map show stmts) ++ "\n}"

instance Show Stmt where
  show (Decl name _) = "Decl: " ++ name
  show (Init name e _) = "Init: " ++ name ++ " = " ++ show e
  show (Asgn name op e _) =
    "Assign: " ++ name ++ " " ++ show' op ++ " " ++ show e
    where
      show' (Just o) = show o ++ "="
      show' Nothing = "="
  show (Ret e _) = "Return: " ++ show e

instance Show Expr where
  show (IntExpr i _) = show i
  show (Ident name _) = name
  show (UnExpr op e) = "(" ++ show op ++ " " ++ show e ++ ")"
  show (BinExpr op lhs rhs) =
    "(" ++ show lhs ++ " " ++ show op ++ " " ++ show rhs ++ ")"

instance Show IntLiteral where
  show (DecLit i) = show i
  show (HexLit i) = "0x" ++ showHex i ""

instance Show Op where
  show Mul = "*"
  show Add = "+"
  show Sub = "-"
  show Div = "/"
  show Neg = "-"
  show Mod = "%"
  show Nop = "[nop]"

showAsgnOp :: AsgnOp -> String
showAsgnOp (Just op) = " " ++ show op ++ "= "
showAsgnOp _ = " = "
