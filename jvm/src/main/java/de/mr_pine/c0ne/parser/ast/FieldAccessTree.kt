package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.parser.type.StructType
import de.mr_pine.c0ne.parser.visitor.Visitor

data class FieldAccessTree(val structValue: ExpressionTree, val member: NameTree) : LValueTree {
    override val span = structValue.span merge member.span
    override val type
        get() =
            (structValue.type as StructType).fieldTypes[member.name]!!

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }
}
