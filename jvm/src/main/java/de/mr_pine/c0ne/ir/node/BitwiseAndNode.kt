package de.mr_pine.c0ne.ir.node

class BitwiseAndNode(block: Block, left: Node, right: Node) : BinaryOperationNode(block, left, right) {
    override fun equals(other: Any?): Boolean {
        return commutativeEquals(this, other)
    }

    override fun hashCode(): Int {
        return commutativeHashCode(this)
    }
}
