{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Compile.IR.SSA (ssaTransform, SSAGraph(..), Node(..), Value(..), BinaryOperation(..), maybeSideEffect) where

import Compile.AST (AST, Expr (BinExpr, Ident, IntExpr, UnExpr), Identifier, Op (Neg), Stmt (..))
import qualified Compile.AST as AST
import Control.Monad.State (MonadState (get, put), State, gets, execState)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set

data Block
    = FunctionBody {name :: Identifier, id :: Int}
    | SubBlock {id :: Int, parent :: Block}
    deriving (Ord, Eq, Show)

data Node
    = Value {value :: Value, block :: Block, id :: Int}
    | Start {block :: Block, id :: Int}
    | Return {result :: Node, sideEffect :: Node, block :: Block, id :: Int}
    deriving (Eq, Show)

instance Ord Node where
  compare n1 n2 = compare n1.id n2.id

data Value = ConstInt Integer | BinaryOperation BinaryOperation deriving (Eq, Show)

data BinaryOperation
    = Add {left :: Node, right :: Node}
    | Sub {left :: Node, right :: Node}
    | Mul {left :: Node, right :: Node}
    | Mod {left :: Node, right :: Node, sideEffect :: Node}
    | Div {left :: Node, right :: Node, sideEffect :: Node}
    deriving (Ord, Eq)

instance Show BinaryOperation where
    show (Add l r)   = "Add {leftId=" ++ show l.id ++ ", rightId=" ++ show r.id ++ "}"
    show (Sub l r)   = "Sub {leftId=" ++ show l.id ++ ", rightId=" ++ show r.id ++ "}"
    show (Mul l r)   = "Mul {leftId=" ++ show l.id ++ ", rightId=" ++ show r.id ++ "}"
    show (Mod l r s) = "Mod {leftId=" ++ show l.id ++ ", rightId=" ++ show r.id ++ ", sideEffectId=" ++ show s.id ++ "}"
    show (Div l r s) = "Div {leftId=" ++ show l.id ++ ", rightId=" ++ show r.id ++ ", sideEffectId=" ++ show s.id ++ "}"

maybeSideEffect :: Node -> Maybe Node
maybeSideEffect (Value (BinaryOperation o@(Div {})) _ _) = Just o.sideEffect
maybeSideEffect (Value (BinaryOperation o@(Mod {})) _ _) = Just o.sideEffect
maybeSideEffect n@(Return {}) = Just n.sideEffect
maybeSideEffect _ = Nothing

type Successors = Map.Map Node (Set.Set Node)
type ValueMapping = Map.Map Identifier Node

data SSATranslationState = SSATranslationState {currentBlock :: Block, largestBlockId :: Int, largestNodeId :: Int, currentSideEffectNode :: Node, translationSuccessors :: Successors, currentReturnNodes :: [Node], valueMapping :: ValueMapping}
type SSATranslation a = State SSATranslationState a

data SSAGraph = SSAGraph {successors :: Successors, returnNodes :: [Node]} deriving (Show)

ssaTransform :: Identifier -> AST -> SSAGraph
ssaTransform functionName ast = SSAGraph (translationSuccessors resultState) (currentReturnNodes resultState)
  where
    astTranslation = translateAst ast
    initialBlock = FunctionBody functionName 0
    startNode = Start initialBlock 0
    initialFunctionState = SSATranslationState initialBlock 0 0 startNode Map.empty [] Map.empty

    resultState = execState astTranslation initialFunctionState

getCurrentBlock :: SSATranslation Block
getCurrentBlock = gets currentBlock

addSuccessor :: Node -> Node -> SSATranslation ()
addSuccessor target successor = do
    state <- get
    let successors = translationSuccessors state
    let updated = Map.alter (Just . Set.insert successor . fromMaybe Set.empty) target successors
    put state{translationSuccessors = updated}
    return ()

assignValue :: Identifier -> Node -> SSATranslation Node
assignValue ident node = do
    state <- get
    let mapping = valueMapping state
    put state{valueMapping = Map.insert ident node mapping}
    return node

insertSideEffect :: Node -> SSATranslation ()
insertSideEffect node = do
    state <- get
    put state{currentSideEffectNode = node}

createNode :: (Block -> Int -> Node) -> SSATranslation Node
createNode c = do
    block <- getCurrentBlock
    nodeId <- gets largestNodeId
    state <- get
    put state{largestNodeId=nodeId + 1}
    return $ c block (nodeId + 1)

translateAst :: AST -> SSATranslation Node
translateAst (AST.Block stmts _) = last . catMaybes <$> mapM translateStatements stmts

translateStatements :: Stmt -> SSATranslation (Maybe Node)
translateStatements (Decl _ _) = return Nothing
translateStatements (Init identifier expr _) = Just <$> translateAssignment identifier expr
translateStatements (Asgn identifier Nothing expr _) = Just <$> translateAssignment identifier expr
translateStatements (Asgn identifier (Just op) expr sourcePos) = Just <$> translateAssignment identifier (BinExpr op (Ident identifier sourcePos) expr)
translateStatements (Ret expr _) = do
    value <- translateExpression expr
    sideEffect <- gets currentSideEffectNode
    returnNode <- createNode $ Return value sideEffect
    current <- get
    put current{currentReturnNodes=currentReturnNodes current ++ [returnNode]}
    addSuccessor value returnNode
    return $ Just returnNode

translateAssignment :: Identifier -> Expr -> SSATranslation Node
translateAssignment identifier expr = translateExpression expr >>= assignValue identifier

translateExpression :: Expr -> SSATranslation Node
translateExpression (IntExpr lit _) = createNode $ Value (ConstInt lit.value)
translateExpression (Ident ident _) = gets ((Map.! ident) . valueMapping)
translateExpression (UnExpr Neg expr) = do
    constZeroNode <- createNode $ Value (ConstInt 0)
    rightNode <- translateExpression expr
    exprNode <- createNode $ Value (BinaryOperation (Sub constZeroNode rightNode))
    addSuccessor constZeroNode exprNode
    addSuccessor rightNode exprNode
    return exprNode
translateExpression (BinExpr op lexpr rexpr) | op == AST.Sub || op == AST.Add || op == AST.Mul = do
    leftNode <- translateExpression lexpr
    rightNode <- translateExpression rexpr
    exprNode <- createNode $ Value (BinaryOperation (operandMapping op leftNode rightNode))
    addSuccessor leftNode exprNode
    addSuccessor rightNode exprNode
    return exprNode
  where
    operandMapping AST.Sub = Sub
    operandMapping AST.Add = Add
    operandMapping AST.Mul = Mul
translateExpression (BinExpr op lexpr rexpr) | op == AST.Mod || op == AST.Div = do
    leftNode <- translateExpression lexpr
    rightNode <- translateExpression rexpr
    sideEffect <- gets currentSideEffectNode
    exprNode <- createNode $ Value (BinaryOperation (operandMapping op leftNode rightNode sideEffect))
    addSuccessor leftNode exprNode
    addSuccessor rightNode exprNode
    insertSideEffect exprNode
    return exprNode
  where
    operandMapping AST.Div = Div
    operandMapping AST.Mod = Mod
translateExpression expr = error ("How do I translate " ++ show expr ++ "?")
