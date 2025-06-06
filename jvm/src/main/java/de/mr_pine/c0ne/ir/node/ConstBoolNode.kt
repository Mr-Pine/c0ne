package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.visitor.SSAVisitor

class ConstBoolNode(block: Block, val value: Boolean) : Node(block) {

    override fun equals(other: Any?): Boolean {
        if (other is ConstBoolNode) {
            return this.block == other.block && other.value == this.value
        }
        return false
    }

    override fun hashCode(): Int {
        return value.hashCode()
    }

    override fun info(): String {
        return "[$value]"
    }

    override fun <R> accept(visitor: SSAVisitor<R>): R {
        return visitor.visit(this)
    }
}
