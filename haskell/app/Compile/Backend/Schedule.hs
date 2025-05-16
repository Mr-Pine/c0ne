module Compile.Backend.Schedule (schedule, Schedule) where

import Compile.IR.SSA (Node (Return, Start, Value), SSAGraph (SSAGraph), Value (BinaryOperation, ConstInt))
import qualified Compile.IR.SSA as SSA
import Control.Monad.State (MonadState (get, put), State, execState, forM_, void, unless, gets)
import Data.List.NonEmpty (groupWith, head)
import qualified Data.Set as Set
import Prelude hiding (head)

type Schedule = [Node]
schedule :: SSAGraph -> Schedule
schedule (SSAGraph successors returnNodes) = graphSchedule $ execState (scheduleBasicBlock exitPoint) initialState
  where
    initialState = ScheduleConstructionState Set.empty []
    exitPoints = map head $ groupWith SSA.block returnNodes
    [exitPoint] = exitPoints

data ScheduleConstructionState = ScheduleConstructionState {visited :: Set.Set Node, graphSchedule :: [Node]}
type ScheduleConstruction a = State ScheduleConstructionState a

addNode :: Node -> ScheduleConstruction ()
addNode node = do
    curr <- get
    let newVisited = Set.insert node (visited curr)
    let expandedSchedule = node : graphSchedule curr
    put curr{visited = newVisited, graphSchedule = expandedSchedule}

isScheduled :: Node -> ScheduleConstruction Bool
isScheduled node = do
    visitedNodes <- gets visited
    return $ node `Set.member` visitedNodes

scheduleBasicBlock :: Node -> ScheduleConstruction ()
scheduleBasicBlock node@(Start _) = addNode node
scheduleBasicBlock node@(Return result sideEffect _) = addNode node >> scheduleBasicBlock result >> scheduleBasicBlock sideEffect
scheduleBasicBlock node@(Value val _) = scheduleValue val
  where
    scheduleValue (ConstInt _) = addNode node
    scheduleValue (BinaryOperation binop) = do
        alreadyScheduled <- isScheduled node
        unless alreadyScheduled $ do
            addNode node
            scheduleBasicBlock $ SSA.right binop
            scheduleBasicBlock $ SSA.left binop
            let sideeffect = SSA.maybeSideEffect node
            forM_ sideeffect scheduleBasicBlock
