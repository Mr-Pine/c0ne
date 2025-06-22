package de.mr_pine.c0ne.analysis

import de.mr_pine.c0ne.backend.Schedule
import de.mr_pine.c0ne.ir.IrGraph
import de.mr_pine.c0ne.ir.node.Block
import de.mr_pine.c0ne.ir.node.Node

fun IrGraph.nodesInControlFlowOrder(): List<Node> {
    val controlFlowOrder = mutableListOf<Node>()
    scan(endBlock, mutableSetOf(endBlock), controlFlowOrder)
    return controlFlowOrder
}

private fun scan(node: Node, visited: MutableSet<Node>, controlFlowOrder: MutableList<Node>) {
    for (predecessor in node.predecessors()) {
        if (predecessor !in visited) {
            visited.add(predecessor)
            scan(predecessor, visited, controlFlowOrder)
        }
    }
    controlFlowOrder.add(node)
}

abstract class BackwardsControlFlow<InValue, OutValue>(schedule: Schedule) :
    BackwardsFlow<InValue, OutValue>(schedule) {

    override fun predecessors(nodeInBlock: NodeInBlock): List<NodeInBlock> {
        val blockSchedule = schedule.blockSchedules[nodeInBlock.block]!!
        val nodeIndex = blockSchedule.nodeOrder.indexOf(nodeInBlock.node)
        if (nodeIndex != 0) return listOf(NodeInBlock(blockSchedule.nodeOrder[nodeIndex - 1], nodeInBlock.block))
        val predecessorBlocks = nodeInBlock.block.predecessors().map { it.block }
        val predecessors = predecessorBlocks.map { NodeInBlock(schedule.blockSchedules[it]!!.nodeOrder.last(), it) }
        return predecessors
    }

    override fun successors(nodeInBlock: NodeInBlock): List<NodeInBlock> {
        val blockSchedule = schedule.blockSchedules[nodeInBlock.block]!!
        val nodeIndex = blockSchedule.nodeOrder.indexOf(nodeInBlock.node)
        if (nodeIndex != blockSchedule.nodeOrder.lastIndex) return listOf(
            NodeInBlock(
                blockSchedule.nodeOrder[nodeIndex + 1],
                nodeInBlock.block
            )
        )
        val successorBlocks = schedule.blockOrder.filter { it.predecessors().any { it.block == nodeInBlock.block } }
        val successors = successorBlocks.mapNotNull {
            schedule.blockSchedules[it]?.let { blockSchedule ->
                NodeInBlock(
                    blockSchedule.nodeOrder.first(),
                    it
                )
            }
        }
        return successors
    }
}