package de.mr_pine.c0ne.ir.optimize

import de.mr_pine.c0ne.ir.GraphConstructor
import de.mr_pine.c0ne.ir.node.AddNode
import de.mr_pine.c0ne.ir.node.BinaryOperationNode
import de.mr_pine.c0ne.ir.node.ConstBoolNode
import de.mr_pine.c0ne.ir.node.ConstIntNode
import de.mr_pine.c0ne.ir.node.DivNode
import de.mr_pine.c0ne.ir.node.EqualsNode
import de.mr_pine.c0ne.ir.node.LessThanEqNode
import de.mr_pine.c0ne.ir.node.LessThanNode
import de.mr_pine.c0ne.ir.node.ModNode
import de.mr_pine.c0ne.ir.node.MulNode
import de.mr_pine.c0ne.ir.node.Node
import de.mr_pine.c0ne.ir.node.SubNode

class ConstantFolding : Optimizer {
    context(constructor: GraphConstructor)
    override fun transform(node: Node): Node {

        return when (node) {
            is LessThanNode if (node.left is ConstIntNode && node.right is ConstIntNode) -> foldConstantLt(node)
            is LessThanEqNode if (node.right is ConstIntNode && node.left is ConstIntNode) -> foldConstantLte(node)
            is EqualsNode -> foldConstantEq(node)
            is BinaryOperationNode if (node.left is ConstIntNode && node.right is ConstIntNode) -> foldConstantBinop(
                node
            )

            else -> node
        }
    }

    private fun foldConstantLt(node: LessThanNode): Node {
        val left = (node.left as ConstIntNode).value
        val right = (node.right as ConstIntNode).value
        return ConstBoolNode(node.graph.startBlock, left < right)
    }

    private fun foldConstantLte(node: LessThanEqNode): Node {
        val left = (node.left as ConstIntNode).value
        val right = (node.right as ConstIntNode).value
        return ConstBoolNode(node.graph.startBlock, left <= right)
    }

    private fun foldConstantEq(node: EqualsNode): Node {
        if (node.left == node.right) return ConstBoolNode(node.graph.startBlock, true)


        if (node.left is ConstIntNode && node.right is ConstIntNode) return ConstBoolNode(
            node.graph.startBlock,
            (node.left as ConstIntNode).value == (node.right as ConstIntNode).value
        )
        if (node.left is ConstBoolNode && node.right is ConstBoolNode) return ConstBoolNode(
            node.graph.startBlock,
            (node.left as ConstBoolNode).value == (node.right as ConstBoolNode).value
        )
        return node
    }

    private fun foldConstantBinop(node: BinaryOperationNode): Node {
        val left = (node.left as ConstIntNode).value
        val right = (node.right as ConstIntNode).value
        return when (node) {
            is AddNode -> ConstIntNode(node.graph.startBlock, left + right)
            is MulNode -> ConstIntNode(node.graph.startBlock, left * right)
            is SubNode -> ConstIntNode(node.graph.startBlock, left - right)
            is DivNode -> if (right == 0) {
                DivNode(node.block, node.right, node.right, node.sideEffect)
            } else if (left == Int.MIN_VALUE && right == -1) node else ConstIntNode(node.graph.startBlock, left / right)

            is ModNode -> if (right == 0) {
                DivNode(node.block, node.right, node.right, node.sideEffect)
            } else if (left == Int.MIN_VALUE && right == -1) node else ConstIntNode(node.graph.startBlock, left % right)

            else -> node // TODO
        }
    }
}