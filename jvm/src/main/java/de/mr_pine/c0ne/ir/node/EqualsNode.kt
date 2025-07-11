package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.visitor.SSAVisitor

class EqualsNode(block: Block, left: Node, right: Node, val size: Int) : BinaryOperationNode(block, left, right) {
    override fun equals(other: Any?): Boolean {
        return commutativeEquals(this, other)
    }

    override fun hashCode(): Int {
        return commutativeHashCode(this)
    }

    override fun <R> accept(visitor: SSAVisitor<R>): R {
        return visitor.visit(this)
    }
}
