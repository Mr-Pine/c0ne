package de.mr_pine.c0ne.ir.optimize

import de.mr_pine.c0ne.ir.GraphConstructor
import de.mr_pine.c0ne.ir.node.*

class CondJumpEliding : Optimizer {
    val condMap = mutableMapOf<JumpNode, Boolean>()

    context(constructor: GraphConstructor)
    override fun transform(node: Node): Node {
        return when (node) {
            is IfNode if node.condition is ConstBoolNode -> {
                val jump = JumpNode(node.block)
                condMap[jump] = (node.condition as ConstBoolNode).value
                jump
            }

            is ProjNode if node.predecessor(0) in condMap -> {
                val jump = node.predecessor(0) as JumpNode
                node.removePredecessor(jump)
                val isTrueBranch = node.projectionInfo == ProjNode.SimpleProjectionInfo.IF_TRUE
                if (isTrueBranch == condMap[jump]) {
                    jump
                } else {
                    UndefNode(node.block)
                }
            }

            else if node.block.predecessors().let{ it.size == 1 && it[0] is UndefNode } -> UndefNode(constructor.graph.startBlock)
            else -> node
        }
    }
}