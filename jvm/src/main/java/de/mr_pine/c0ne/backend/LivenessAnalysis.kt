package de.mr_pine.c0ne.backend

import de.mr_pine.c0ne.analysis.BackwardsControlFlow
import de.mr_pine.c0ne.analysis.nodesInControlFlowOrder
import de.mr_pine.c0ne.ir.IrGraph
import de.mr_pine.c0ne.ir.node.BinaryOperationNode
import de.mr_pine.c0ne.ir.node.Block
import de.mr_pine.c0ne.ir.node.ConstBoolNode
import de.mr_pine.c0ne.ir.node.ConstIntNode
import de.mr_pine.c0ne.ir.node.ExitNode
import de.mr_pine.c0ne.ir.node.IfNode
import de.mr_pine.c0ne.ir.node.Node
import de.mr_pine.c0ne.ir.node.Phi
import de.mr_pine.c0ne.ir.node.ProjNode
import de.mr_pine.c0ne.ir.node.ReturnNode
import de.mr_pine.c0ne.ir.node.StartNode
import de.mr_pine.c0ne.ir.node.UnaryOperationNode
import de.mr_pine.c0ne.ir.node.UndefNode

class LivenessAnalysis(private val startBlock: Block, schedule: Schedule) :
    BackwardsControlFlow<Set<Node>, Set<Node>>(schedule) {
    override fun computeInValue(
        nodeInBlock: NodeInBlock,
        outValue: Set<Node>
    ): Set<Node> {
        val defined = defined(nodeInBlock.node)
        val used = used(nodeInBlock)
        val liveIn = (outValue - defined) union used
        return liveIn
    }

    override fun analyze() {
        super.analyze()
        val start = NodeInBlock(schedule.blockSchedules[startBlock]!!.nodeOrder.first(), startBlock)
        assert(result[start]!!.outValue.isEmpty()) { "Something is alive at program start. Not good" }
    }

    override fun computeOutValue(
        nodeInBlock: NodeInBlock,
        inputs: List<Set<Node>?>
    ): Set<Node> {
        val liveOut = inputs.filterNotNull().reduceOrNull(Set<Node>::union) ?: setOf()
        return liveOut
    }

    private fun defined(node: Node): Set<Node> {
        return when (node) {
            is BinaryOperationNode, is UnaryOperationNode, is Phi, is ConstIntNode, is ConstBoolNode -> setOf(node)
            else -> setOf()
        }
    }

    private fun used(nodeInBlock: NodeInBlock): Set<Node> {
        val node = nodeInBlock.node
        return when (node) {
            is BinaryOperationNode -> setOf(node.left, node.right)
            is UnaryOperationNode -> setOf(node.value)
            is Phi -> {
                val predecessorIndex = node.block.predecessors().indexOfFirst { it.block == nodeInBlock.block }
                setOf(node[predecessorIndex])
            }
            is ReturnNode -> setOf(node.result)
            is IfNode -> setOf(node.condition)
            is ExitNode, is ProjNode, is ConstIntNode, is ConstBoolNode, is Block, is StartNode, is UndefNode -> setOf()
        }
    }

}