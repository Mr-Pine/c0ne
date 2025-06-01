package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.visitor.SSAVisitor

class StartNode(block: Block) : Node(block) {
    override fun <R> accept(visitor: SSAVisitor<R>): R {
        return visitor.visit(this)
    }
}
