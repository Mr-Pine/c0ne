package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.util.NodeSupport
import de.mr_pine.c0ne.ir.visitor.SSAVisitor
import de.mr_pine.c0ne.parser.symbol.Name

class CallNode(block: Block, val target: Name, arguments: List<Node>, sideEffect: Node) :
    Node(block, *(arguments + sideEffect).toTypedArray()) {

    val sideEffectIndex = arguments.size

    override fun <R> accept(visitor: SSAVisitor<R>): R {
        return visitor.visit(this)
    }

    val arguments
        get() = (0..<predecessors().size).map { NodeSupport.predecessorSkipSimpleProj(this, it) }
            .filterIndexed { index, _ -> index != sideEffectIndex }

    val sideEffect
        get() = NodeSupport.predecessorSkipSimpleProj(this, sideEffectIndex)
}
