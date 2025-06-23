package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.util.NodeSupport
import de.mr_pine.c0ne.ir.visitor.SSAVisitor

class ReturnNode(block: Block, sideEffect: Node, result: Node) : ExitNode(block, sideEffect, result) {
    companion object {
        const val SIDE_EFFECT: Int = 0
        const val RESULT: Int = 1
    }

    override fun <R> accept(visitor: SSAVisitor<R>): R {
        return visitor.visit(this)
    }

    val result
        get() = NodeSupport.predecessorSkipSimpleProj(this, RESULT)
}
