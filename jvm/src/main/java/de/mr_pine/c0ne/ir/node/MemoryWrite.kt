package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.util.NodeSupport
import de.mr_pine.c0ne.ir.visitor.SSAVisitor

class MemoryWrite(block: Block, val base: Node, val offset: Node?, val constantOffset: Int, val value: Node, sideEffect: Node) :
    Node(block, *listOfNotNull(base, offset, sideEffect, value).toTypedArray()) {

    override fun <R> accept(visitor: SSAVisitor<R>): R {
        return visitor.visit(this)
    }

    val sideEffectIndex: Int = if (offset == null) 1 else 2

    val sideEffect
        get() = NodeSupport.predecessorSkipSimpleProj(this, sideEffectIndex)
}
