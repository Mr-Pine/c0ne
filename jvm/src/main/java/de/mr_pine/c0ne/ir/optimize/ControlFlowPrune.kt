package de.mr_pine.c0ne.ir.optimize

import de.mr_pine.c0ne.ir.IrGraph
import de.mr_pine.c0ne.ir.node.Block

class ControlFlowPrune : FinishPassOptimizer {
    val toRemove = mutableSetOf<Block>()
    val checkedBlocks = mutableSetOf<Block>()

    override fun optimize(irGraph: IrGraph) {
        val blocks = irGraph.allSuccessors.values.flatten().mapNotNull { it as? Block }.toSet()

        for (block in blocks) {
            checkBlock(block, irGraph)
        }

        for (block in toRemove) {
            block.clearPredecessors()
            val successors =
                irGraph.allSuccessors.entries.filter { it.key.block == block }.flatMap { it.value }.map { it.block }
                    .toSet()
            for (successor in successors) {
                block.removeSuccessor(successor, irGraph)
            }
        }
    }

    fun checkBlock(block: Block, irGraph: IrGraph) {
        if (block in checkedBlocks) return
        checkedBlocks.add(block)

        if (block == irGraph.startBlock) return

        for (predecessor in block.predecessors()) {
            checkBlock(predecessor.block, irGraph)
        }

        if (block.predecessors().map { it.block }.all { it in toRemove }) toRemove.add(block)
    }
}