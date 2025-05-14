module Compile.Backend.X86.RegAlloc (allocate, RegisterMap, Register)
where

import Compile.AST (AST (Block), Identifier, Stmt (Decl, Init))
import Control.Monad.State (MonadState (get, put), State, execState)
import qualified Data.Map as Map

data Register = EAX | EBX | ECX | EDX | ESI | EDI | RSP | RBP | R8D | R9D | R10D | R11D | R12D | R13D | R14D | R15D | OverflowSlot {index :: Int}

allocateableRegisters :: [Register]
allocateableRegisters = [EAX, EBX, ECX, ESI, EDI, R8D, R9D, R10D, R11D, R12D, R13D, R14D] ++ map OverflowSlot [1 ..]

type RegisterMap = Map.Map Identifier Register
data RegAllocState = RegAllocState {remainingRegisters :: [Register], regMap :: RegisterMap}
type RegAlloc a = State RegAllocState a

nextReg :: RegAlloc Register
nextReg = do
    curr <- get
    let remaining = remainingRegisters curr
    let next = head remaining
    put curr{remainingRegisters = tail remaining}
    return next

addMapping :: Identifier -> Register -> RegAlloc ()
addMapping ident reg = do
    curr <- get
    let registerMap = regMap curr
    put curr{regMap = Map.insert ident reg registerMap}

allocate :: AST -> RegisterMap
allocate (Block statements _) = regMap $ execState (allocateBlock statements) initialState
  where
    initialState = RegAllocState allocateableRegisters Map.empty

allocateBlock :: [Stmt] -> RegAlloc ()
allocateBlock = mapM_ allocateStatement

allocateStatement :: Stmt -> RegAlloc ()
allocateStatement (Decl ident _) = do
    reg <- nextReg
    addMapping ident reg
allocateStatement (Init ident _ _) = do
    reg <- nextReg
    addMapping ident reg
allocateStatement _ = return ()
