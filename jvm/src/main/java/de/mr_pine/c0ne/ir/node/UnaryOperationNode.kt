package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.util.NodeSupport

sealed class UnaryOperationNode : Node {
    protected constructor(block: Block, value: Node) : super(block, value)

    override fun equals(other: Any?): Boolean {
        if (other !is UnaryOperationNode) {
            return false
        }
        return other.javaClass == this.javaClass && this.predecessor(VALUE) === other.predecessor(VALUE)
    }

    override fun hashCode(): Int {
        return (predecessorHash(this, VALUE) * 31) xor this.javaClass.hashCode()
    }

    companion object {
        const val VALUE: Int = 0
    }

    val value
        get() = NodeSupport.predecessorSkipProj(this, VALUE)
}
