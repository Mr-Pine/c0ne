package de.mr_pine.c0ne.backend

import de.mr_pine.c0ne.analysis.BackwardsControlFlow
import de.mr_pine.c0ne.ir.left
import de.mr_pine.c0ne.ir.result
import de.mr_pine.c0ne.ir.right
import edu.kit.kastel.vads.compiler.ir.node.BinaryOperationNode
import edu.kit.kastel.vads.compiler.ir.node.Node
import edu.kit.kastel.vads.compiler.ir.node.ReturnNode

class LivenessAnalysis: BackwardsControlFlow<Set<Node>>() {
    override fun computeOutput(
        node: Node,
        inputs: List<Set<Node>?>
    ): Set<Node> {
        val liveOut = inputs.filterNotNull().reduceOrNull (Set<Node>::union) ?: setOf()
        val defined = defined(node)
        val used = used(node)
        val liveIn = (liveOut - defined) union used
        return liveIn
    }

    private fun defined(node: Node): Set<Node> {
        return when (node) {
            is BinaryOperationNode -> setOf(node)
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