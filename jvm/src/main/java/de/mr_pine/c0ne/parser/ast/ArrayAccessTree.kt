package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.parser.type.ArrayType
import de.mr_pine.c0ne.parser.type.Type
import de.mr_pine.c0ne.parser.visitor.Visitor

data class ArrayAccessTree(val arrayValue: ExpressionTree, val index: ExpressionTree, override val span: Span) : LValueTree {
    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        TODO("Not yet implemented")
    }

    override val type: Type
        get() = (arrayValue.type as ArrayType).baseType
}
