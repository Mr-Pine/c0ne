package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.util.NodeSupport

sealed class BinaryOperationNode : Node {
    protected constructor(block: Block, left: Node, right: Node) : super(block, left, right)

    protected constructor(block: Block, left: Node, right: Node, sideEffect: Node) : super(
        block,
        left,
        right,
        sideEffect
    )

    override fun equals(other: Any?): Boolean {
        if (other !is BinaryOperationNode) {
            return false
        }
        return other.javaClass == this.javaClass && this.predecessor(LEFT) === other.predecessor(LEFT) && this.predecessor(
            RIGHT
        ) === other.predecessor(RIGHT) && this.block == other.block
    }

    override fun hashCode(): Int {
        return block.hashCode() + 31 * ((predecessorHash(this, LEFT) * 31 + predecessorHash(this, RIGHT)) xor this.javaClass.hashCode())
    }

    companion object {
        const val LEFT: Int = 0
        const val RIGHT: Int = 1
        const val SIDE_EFFECT: Int = 2

        @JvmStatic
        protected fun commutativeHashCode(node: BinaryOperationNode): Int {
            var h = node.block.hashCode()
            // commutative operation: we want h(op(x, y)) == h(op(y, x))
            h += 31 * (predecessorHash(node, LEFT) xor predecessorHash(node, RIGHT))
            return h
        }

        @JvmStatic
        protected fun commutativeEquals(a: BinaryOperationNode, bObj: Any?): Boolean {
            if (bObj !is BinaryOperationNode) {
                return false
            }
            if (a.javaClass != bObj.javaClass) {
                return false
            }
            if (a.block != bObj.block) {
                return false
            }
            if (a.predecessor(LEFT) === bObj.predecessor(LEFT) && a.predecessor(RIGHT) === bObj.predecessor(RIGHT)) {
                return true
            }
            // commutative operation: op(x, y) == op(y, x)
            return a.predecessor(LEFT) === bObj.predecessor(RIGHT) && a.predecessor(RIGHT) === bObj.predecessor(LEFT)
        }
    }

    val left
        get() = NodeSupport.predecessorSkipProj(this, LEFT)
    val right
        get() = NodeSupport.predecessorSkipProj(this, RIGHT)
}
