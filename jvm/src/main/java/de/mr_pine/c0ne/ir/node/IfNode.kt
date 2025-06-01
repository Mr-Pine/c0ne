package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.visitor.SSAVisitor

class IfNode(block: Block, condition: Node): ExitNode(block, condition) {
    override fun accept(visitor: SSAVisitor) {
        visitor.visit(this)
    }
}