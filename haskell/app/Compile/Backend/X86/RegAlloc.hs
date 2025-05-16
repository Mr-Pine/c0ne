module Compile.Backend.X86.RegAlloc (allocate, RegisterMap, overflowCount, Register, print)
where

import Control.Monad.State (MonadState (get, put), State, execState)
import qualified Data.Map as Map
import Compile.Backend.Schedule (Schedule)
import Compile.IR.SSA (Node)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List.Extra (lower)
import Prelude hiding (print)
import Safe.Foldable (maximumMay)

data Register = EAX | EBX | ECX | EDX | ESI | EDI | RSP | RBP | R8D | R9D | R10D | R11D | R12D | R13D | R14D | R15D | OverflowSlot {index :: Int} deriving (Show, Eq)

print :: Register -> String
print (OverflowSlot idx) = "DWORD PTR [RSP + " ++ show (idx * 4) ++ "]"
print reg = lower $ show reg

allocateableRegisters :: [Register]
allocateableRegisters = [EAX, EBX, ECX, ESI, EDI, R8D, R9D, R10D, R11D, R12D, R13D, R14D] ++ map OverflowSlot [1 ..]

type RegisterMap = Map.Map Node Register
data RegAllocState = RegAllocState {remainingRegisters :: [Register], regMap :: RegisterMap}
type RegAlloc a = State RegAllocState a

overflowCount :: RegisterMap -> Int
overflowCount = fromMaybe 0 . maximumMay . mapMaybe getIndex . Map.elems
    where
        getIndex (OverflowSlot idx) = Just idx
        getIndex _ = Nothing

nextReg :: RegAlloc Register
nextReg = do
    curr <- get
    let remaining = remainingRegisters curr
    let next = head remaining
    put curr{remainingRegisters = tail remaining}
    return next

addMapping :: Node -> Register -> RegAlloc ()
addMapping node reg = do
    curr <- get
    let registerMap = regMap curr
    put curr{regMap = Map.insert node reg registerMap}

allocate :: Schedule -> RegisterMap
allocate schedule = regMap $ execState (allocateSchedule schedule) initialState
  where
    initialState = RegAllocState allocateableRegisters Map.empty

allocateSchedule :: Schedule -> RegAlloc ()
allocateSchedule = mapM_ allocateNode

allocateNode :: Node -> RegAlloc ()
allocateNode node = nextReg >>= addMapping node
