package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.util.NodeSupport
import de.mr_pine.c0ne.ir.visitor.SSAVisitor

class DivNode(block: Block, left: Node, right: Node, sideEffect: Node) :
    BinaryOperationNode(block, left, right, sideEffect) {
    override fun equals(other: Any?): Boolean {
        // side effect, must be very careful with value numbering.
        // this is the most conservative approach
        return other === this
    }

    companion object {
        const val SIDE_EFFECT: Int = 2
    }

    override fun hashCode(): Int {
        return super.hashCode()
    }

    override fun <R> accept(visitor: SSAVisitor<R>): R {
        return visitor.visit(this)
    }

    val sideEffect
        get() = NodeSupport.predecessorSkipSimpleProj(this, SIDE_EFFECT)
}
