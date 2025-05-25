package de.mr_pine.c0ne.ir.optimize

import de.mr_pine.c0ne.ir.left
import de.mr_pine.c0ne.ir.right
import de.mr_pine.c0ne.ir.sideEffect
import edu.kit.kastel.vads.compiler.ir.node.AddNode
import edu.kit.kastel.vads.compiler.ir.node.BinaryOperationNode
import edu.kit.kastel.vads.compiler.ir.node.ConstIntNode
import edu.kit.kastel.vads.compiler.ir.node.DivNode
import edu.kit.kastel.vads.compiler.ir.node.ModNode
import edu.kit.kastel.vads.compiler.ir.node.MulNode
import edu.kit.kastel.vads.compiler.ir.node.Node
import edu.kit.kastel.vads.compiler.ir.node.SubNode

class ConstantFolding: Optimizer {
    override fun transform(node: Node): Node {

        return when(node) {
            is BinaryOperationNode if (node.left is ConstIntNode && node.right is ConstIntNode) -> foldConstants(node)
            else -> node
        }
    }

    private fun foldConstants(node: BinaryOperationNode): Node {
        val left = (node.left as ConstIntNode).value()
        val right = (node.right as ConstIntNode).value()
        return when (node) {
            is AddNode -> ConstIntNode(node.block(), left + right)
            is MulNode -> ConstIntNode(node.block(), left * right)
            is SubNode -> ConstIntNode(node.block(), left - right)
            is DivNode -> if (right == 0) {
                DivNode(node.block(), node.right, node.right, node.sideEffect)
            } else if (left == Int.MIN_VALUE && right == -1) node else ConstIntNode(node.block(), left / right)
            is ModNode -> if (right == 0) {
                DivNode(node.block(), node.right, node.right, node.sideEffect)
            } else if (left == Int.MIN_VALUE && right == -1) node else ConstIntNode(node.block(), left % right)
        }
    }
}