package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.util.NodeSupport
import de.mr_pine.c0ne.ir.visitor.SSAVisitor

class MemoryReadNode(
    block: Block,
    base: Node,
    offset: Node?,
    val offsetScale: Int,
    val constantOffset: Int,
    sideEffect: Node
) :
    Node(block, *listOfNotNull(base, offset, sideEffect).toTypedArray()) {

    val hasOffset = offset != null

    override fun <R> accept(visitor: SSAVisitor<R>): R {
        return visitor.visit(this)
    }

    val sideEffectIndex: Int = if (hasOffset) 2 else 1

    companion object {
        const val BASE: Int = 0
        const val OFFSET: Int = 1
    }

    val sideEffect
        get() = NodeSupport.predecessorSkipSimpleProj(this, sideEffectIndex)
    val base
        get() = NodeSupport.predecessorSkipSimpleProj(this, BASE)
    val offset
        get() = if (hasOffset) NodeSupport.predecessorSkipSimpleProj(this, OFFSET) else null
}
