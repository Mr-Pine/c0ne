package de.mr_pine.c0ne.backend

import de.mr_pine.c0ne.analysis.BackwardsControlFlow
import de.mr_pine.c0ne.analysis.nodesInControlFlowOrder
import de.mr_pine.c0ne.ir.left
import de.mr_pine.c0ne.ir.result
import de.mr_pine.c0ne.ir.right
import de.mr_pine.c0ne.ir.IrGraph
import de.mr_pine.c0ne.ir.node.BinaryOperationNode
import de.mr_pine.c0ne.ir.node.ConstIntNode
import de.mr_pine.c0ne.ir.node.Node
import de.mr_pine.c0ne.ir.node.ReturnNode

class LivenessAnalysis : BackwardsControlFlow<Set<Node>, Set<Node>>() {
    override fun computeInValue(
        node: Node,
        outValue: Set<Node>
    ): Set<Node> {
        val defined = defined(node)
        val used = used(node)
        val liveIn = (outValue - defined) union used
        return liveIn
    }

    override fun analyze(graph: IrGraph) {
        super.analyze(graph)
        assert(result[graph.nodesInControlFlowOrder()[0]]!!.outValue.isEmpty()) { "Something is alive at program start. Not good" }
    }

    override fun computeOutValue(
        node: Node,
        inputs: List<Set<Node>?>
    ): Set<Node> {
        val liveOut = inputs.filterNotNull().reduceOrNull(Set<Node>::union) ?: setOf()
        return liveOut
    }

    private fun defined(node: Node): Set<Node> {
        return when (node) {
            is BinaryOperationNode, is ConstIntNode -> setOf(node)
            else -> setOf()
        }
    }

    private fun used(node: Node): Set<Node> {
        return when (node) {
            is BinaryOperationNode -> setOf(node.left, node.right)
            is ReturnNode -> setOf(node.result)
            else -> setOf()
        }
    }

}