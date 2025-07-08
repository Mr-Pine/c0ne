package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.parser.type.ArrayType
import de.mr_pine.c0ne.parser.type.PointerType
import de.mr_pine.c0ne.parser.visitor.Visitor

data class HeapAllocationTree(
    val typeTree: TypeTree,
    val arrayCount: Int?,
    override val span: Span,
) : Tree, StatementTree, ExpressionTree {

    override val type = if (arrayCount == null) {
        PointerType(typeTree.type)
    } else {
        ArrayType(typeTree.type)
    }

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }
}
