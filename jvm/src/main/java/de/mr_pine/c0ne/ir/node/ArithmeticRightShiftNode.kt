package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.visitor.SSAVisitor

class ArithmeticRightShiftNode(block: Block, value: Node, shift: Node) : BinaryOperationNode(block, value, shift) {
    override fun <R> accept(visitor: SSAVisitor<R>): R {
        return visitor.visit(this)
    }
}
