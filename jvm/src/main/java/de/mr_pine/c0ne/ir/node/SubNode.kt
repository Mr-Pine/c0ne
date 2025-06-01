package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.visitor.SSAVisitor

class SubNode(block: Block, left: Node, right: Node) : BinaryOperationNode(block, left, right) {
    override fun accept(visitor: SSAVisitor) {
        visitor.visit(this)
    }
}
