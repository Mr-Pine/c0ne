package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.visitor.SSAVisitor

class BitwiseAndNode(block: Block, left: Node, right: Node) : BinaryOperationNode(block, left, right) {
    override fun equals(other: Any?): Boolean {
        return commutativeEquals(this, other)
    }

    override fun hashCode(): Int {
        return commutativeHashCode(this)
    }

    override fun accept(visitor: SSAVisitor) {
        visitor.visit(this)
    }
}
