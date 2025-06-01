package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.util.NodeSupport
import de.mr_pine.c0ne.ir.visitor.SSAVisitor

class IfNode(block: Block, condition: Node) : ExitNode(block, condition) {
    override fun <R> accept(visitor: SSAVisitor<R>): R {
        return visitor.visit(this)
    }

    companion object {
        const val CONDITION: Int = 0
    }

    val condition
        get() = NodeSupport.predecessorSkipProj(this, CONDITION)
}