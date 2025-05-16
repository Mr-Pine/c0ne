module Compile.IR.SSA (ssaTransform, SSAGraph) where

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
    = Value {value :: Value, block :: Block}
    | Start {block :: Block}
    | Return {result :: Node, block :: Block}
    deriving (Ord, Eq, Show)

data Value = ConstInt Integer | BinaryOperation BinaryOperation deriving (Ord, Eq, Show)

data BinaryOperation
    = Add {left :: Node, right :: Node}
    | Sub {left :: Node, right :: Node}
    | Mod {left :: Node, right :: Node, sideEffect :: Node}
    | Div {left :: Node, right :: Node, sideEffect :: Node}
    deriving (Ord, Eq, Show)

type Successors = Map.Map Node (Set.Set Node)
type ValueMapping = Map.Map Identifier Node

data SSATranslationState = SSATranslationState {currentBlock :: Block, largestBlockId :: Int, currentSideEffectNode :: Node, translationSuccessors :: Successors, currentReturnNodes :: [Node], valueMapping :: ValueMapping}
type SSATranslation a = State SSATranslationState a

data SSAGraph = SSAGraph {successors :: Successors, returnNodes :: [Node]} deriving (Show)

ssaTransform :: Identifier -> AST -> SSAGraph
ssaTransform functionName ast = SSAGraph (translationSuccessors resultState) (currentReturnNodes resultState)
  where
    astTranslation = translateAst ast
    initialBlock = FunctionBody functionName 0
    startNode = Start initialBlock
    initialFunctionState = SSATranslationState initialBlock 0 startNode Map.empty [] Map.empty

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

translateAst :: AST -> SSATranslation Node
translateAst (AST.Block stmts _) = last . catMaybes <$> mapM translateStatements stmts

translateStatements :: Stmt -> SSATranslation (Maybe Node)
translateStatements (Decl _ _) = return Nothing
translateStatements (Init identifier expr _) = Just <$> translateAssignment identifier expr
translateStatements (Asgn identifier Nothing expr _) = Just <$> translateAssignment identifier expr
translateStatements (Asgn identifier (Just op) expr sourcePos) = Just <$> translateAssignment identifier (BinExpr op (Ident identifier sourcePos) expr)
translateStatements (Ret expr _) = do
    value <- translateExpression expr
    block <- getCurrentBlock
    let returnNode = Return value block
    current <- get
    put current{currentReturnNodes=currentReturnNodes current ++ [returnNode]}
    addSuccessor value returnNode
    return $ Just returnNode

translateAssignment :: Identifier -> Expr -> SSATranslation Node
translateAssignment identifier expr = translateExpression expr >>= assignValue identifier

translateExpression :: Expr -> SSATranslation Node
translateExpression (IntExpr value _) = do
    block <- gets currentBlock
    let valueNode = Value (ConstInt value) block
    return valueNode
translateExpression (Ident ident _) = gets ((Map.! ident) . valueMapping)
translateExpression (UnExpr Neg expr) = do
    block <- gets currentBlock
    let constZeroNode = Value (ConstInt 0) block
    rightNode <- translateExpression expr
    let exprNode = Value (BinaryOperation (Sub constZeroNode rightNode)) block
    addSuccessor constZeroNode exprNode
    addSuccessor rightNode exprNode
    return exprNode
translateExpression (BinExpr op lexpr rexpr) | op == AST.Sub || op == AST.Add = do
    block <- gets currentBlock
    leftNode <- translateExpression lexpr
    rightNode <- translateExpression rexpr
    let exprNode = Value (BinaryOperation (operandMapping op leftNode rightNode)) block
    addSuccessor leftNode exprNode
    addSuccessor rightNode exprNode
    return exprNode
  where
    operandMapping AST.Sub = Sub
    operandMapping AST.Add = Add
translateExpression (BinExpr op lexpr rexpr) | op == AST.Mod || op == AST.Div = do
    block <- gets currentBlock
    leftNode <- translateExpression lexpr
    rightNode <- translateExpression rexpr
    sideEffect <- gets currentSideEffectNode
    let exprNode = Value (BinaryOperation (operandMapping op leftNode rightNode sideEffect)) block
    addSuccessor leftNode exprNode
    addSuccessor rightNode exprNode
    insertSideEffect exprNode
    return exprNode
  where
    operandMapping AST.Div = Div
    operandMapping AST.Mod = Mod
