package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.visitor.SSAVisitor

class BitwiseNotNode(block: Block, value: Node) : UnaryOperationNode(block, value) {
    override fun <R> accept(visitor: SSAVisitor<R>): R {
        return visitor.visit(this)
    }
}
