package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.visitor.SSAVisitor

class UndefNode(block: Block): Node(block) {
    override fun accept(visitor: SSAVisitor) {
        visitor.visit(this)
    }
}