package de.mr_pine.c0ne.ir.node

class ConstIntNode(block: Block, val value: Int) : Node(block), ValueNode {

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
}
