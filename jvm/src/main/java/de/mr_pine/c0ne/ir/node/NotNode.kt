package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.visitor.SSAVisitor

class NotNode(block: Block, value: Node) : UnaryOperationNode(block, value) {
    override fun accept(visitor: SSAVisitor) {
        visitor.visit(this)
    }
}
