package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.util.NodeSupport
import de.mr_pine.c0ne.ir.visitor.SSAVisitor

class MemoryRead(block: Block, val base: Node, val offset: Node, val constantOffset: Int, sideEffect: Node) :
    Node(block, base, offset, sideEffect) {

    override fun <R> accept(visitor: SSAVisitor<R>): R {
        return visitor.visit(this)
    }

    companion object {
        const val SIDE_EFFECT: Int = 2
    }

    val sideEffect
        get() = NodeSupport.predecessorSkipSimpleProj(this, SIDE_EFFECT)
}
