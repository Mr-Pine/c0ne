module Compile.Backend.X86.Codegen
()
where
import Compile.Backend.X86.RegAlloc (RegisterMap, allocate)
import Control.Monad.State
import Compile.AST (AST (Block), Stmt)

type CodeGen a = State CodeGenState a

data CodeGenState = CodeGenState
  { regMap :: RegisterMap
  , code :: [String]
  }

codeGen :: AST -> [String]
codeGen block@(Block stmts _) = code $ execState (genBlock stmts) initialState
  where
    registerMap = allocate block
    initialState = CodeGenState registerMap []

genBlock :: [Stmt] -> CodeGen ()
genBlock = mapM_ genStmt

emit :: String -> CodeGen ()
emit instr = modify $ \s -> s {code = code s ++ [instr]}

genStmt:: Stmt -> CodeGen ()
genStmt = error "a"
