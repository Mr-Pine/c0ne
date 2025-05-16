module Compile.Backend.X86.Codegen (codeGen)
where

import Compile.Backend.Schedule (Schedule)
import Compile.Backend.X86.RegAlloc (Register, RegisterMap, allocate, overflowCount, print)
import Compile.IR.SSA (Node (Return, Start, Value), Value (ConstInt, BinaryOperation), BinaryOperation (Add, Sub, Mul, Div, Mod))
import qualified Compile.IR.SSA as SSA
import Control.Monad.State (State, execState, modify, gets)
import qualified Data.Map as Map
import Prelude hiding (print)
import Data.Maybe (fromMaybe)
import Text.Pretty.Simple (pPrint, pShow, pShowNoColor)

type CodeGen a = State CodeGenState a

data CodeGenState = CodeGenState
    { regMap :: RegisterMap
    , code :: [String]
    }

codeGen :: Schedule -> [String]
codeGen schedule = code $ execState (genNodes schedule) initialState
  where
    registerMap = allocate schedule
    initialState = CodeGenState registerMap (prologue $ overflowCount registerMap)

prologue :: Int -> [String]
prologue overflow = [".intel_syntax noprefix", ".global _main", ".text", "", "_main:", "call main", "mov edi, eax", "mov rax, 0x3c", "syscall", "", ".global main", "main:", "ENTER " ++ show (overflow * 4) ++ ", 0"]

registerFor :: Node -> CodeGen Register
registerFor node = do
    registerMap <- gets regMap
    return . fromMaybe (error $ "No reg for " ++ show (pShowNoColor node)) $ registerMap Map.!? node

emit :: String -> CodeGen ()
emit instr = modify $ \s -> s{code = code s ++ [instr]}

genNodes :: [Node] -> CodeGen ()
genNodes = mapM_ genNode

genNode :: Node -> CodeGen ()
genNode (Start _) = return ()
genNode (Return result _ _) = do
    resReg <- registerFor result
    emit $ "MOV eax, " ++ print resReg
    emit "LEAVE"
    emit "RET"
    return ()
genNode node@(Value val _) = genValue val
    where
        genValue (ConstInt int) = do
            target <- registerFor node
            emit $ "MOV " ++ print target ++ ", " ++ show int
        genValue (BinaryOperation binop) = do
            target <- registerFor node
            leftReg <- registerFor $ SSA.left binop
            emit $ "MOV eax, " ++ print leftReg
            rightReg <- registerFor $ SSA.right binop
            genBinop binop rightReg target
        genBinop (Add _ _) rightReg target = do
            emit $ "ADD eax, " ++ print rightReg
            emit $ "MOV " ++ print target ++ ", eax"
        genBinop (Sub _ _) rightReg target = do
            emit $ "SUB eax, " ++ print rightReg
            emit $ "MOV " ++ print target ++ ", eax"
        genBinop (Mul _ _) rightReg target = do
            emit $ "IMUL eax, " ++ print rightReg
            emit $ "MOV " ++ print target ++ ", eax"
        genBinop (Div {}) rightReg target = do
            emit "CDQ"
            emit $ "MOV r15d, " ++ print rightReg
            emit "IDIV r15d"
            emit $ "MOV " ++ print target ++ ", eax"
        genBinop (Mod {}) rightReg target = do
            emit "CDQ"
            emit $ "MOV r15d, " ++ print rightReg
            emit "IDIV r15d"
            emit $ "MOV " ++ print target ++ ", edx"

needsRegister (Value _ _) = True
needsRegister _ = False
