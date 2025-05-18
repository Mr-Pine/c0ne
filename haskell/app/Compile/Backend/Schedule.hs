module Compile.Backend.Schedule (schedule, Schedule) where

import Compile.IR.SSA (Node (Return, Start, Value), SSAGraph (SSAGraph), Value (BinaryOperation, ConstInt))
import qualified Compile.IR.SSA as SSA
import Control.Monad.State (MonadState (get, put), State, execState, forM_, void, unless, gets)
import Data.List.NonEmpty (groupWith, head)
import qualified Data.Set as Set
import Prelude hiding (head)

{-
 - Scheduling Algorithm
 -
 - Do a toposort.
 - As we have much nicer back edges, we do a reverse toposort and reverse the result
 -}

type Schedule = [Node]
schedule :: SSAGraph -> Schedule
schedule (SSAGraph successors returnNodes) = toSchedule toposort
  where
    initialState = ScheduleConstructionState Set.empty []
    exitPoints = map head $ groupWith SSA.block returnNodes
    [exitPoint] = exitPoints

    moveStartToStart xs = filter isStart xs ++ filter (not . isStart) xs
    toSchedule = moveStartToStart . reverse
    toposort = graphSchedule $ execState (scheduleBasicBlock exitPoint) initialState
    isStart (Start {}) = True
    isStart _ = False

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
scheduleBasicBlock node@(Start _ _) = addNode node
scheduleBasicBlock node@(Return result sideEffect _ _) = scheduleBasicBlock result >> scheduleBasicBlock sideEffect >> addNode node
scheduleBasicBlock node@(Value val _ _) = scheduleValue val
  where
    scheduleValue (ConstInt _) = addNode node
    scheduleValue (BinaryOperation binop) = do
        alreadyScheduled <- isScheduled node
        unless alreadyScheduled $ do
            scheduleBasicBlock $ SSA.right binop
            scheduleBasicBlock $ SSA.left binop
            let sideeffect = SSA.maybeSideEffect node
            forM_ sideeffect scheduleBasicBlock
            addNode node
