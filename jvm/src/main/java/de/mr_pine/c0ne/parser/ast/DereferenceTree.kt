package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.parser.type.PointerType
import de.mr_pine.c0ne.parser.visitor.Visitor

data class DereferenceTree(val pointerValue: ExpressionTree, override val span: Span) : LValueTree {
    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        TODO("Not yet implemented")
    }

    override val type
        get() = (pointerValue.type as PointerType).baseType
}
