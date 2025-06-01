package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.visitor.SSAVisitor

class ConstIntNode(block: Block, val value: Int) : Node(block) {

    override fun equals(other: Any?): Boolean {
        if (other is ConstIntNode) {
            return this.block == other.block && other.value == this.value
        }
        return false
    }

    override fun hashCode(): Int {
        return this.value
    }

    override fun info(): String {
        return "[$value]"
    }

    override fun accept(visitor: SSAVisitor) {
        visitor.visit(this)
    }
}
