package de.mr_pine.c0ne.ir.node

class ConstBoolNode(block: Block, val value: Boolean) : Node(block), ValueNode {

    override fun equals(other: Any?): Boolean {
        if (other is ConstBoolNode) {
            return this.block() == other.block() && other.value == this.value
        }
        return false
    }

    override fun hashCode(): Int {
        return value.hashCode()
    }

    override fun info(): String {
        return "[$value]"
    }
}
