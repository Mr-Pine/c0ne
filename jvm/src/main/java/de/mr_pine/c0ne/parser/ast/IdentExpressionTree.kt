package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.parser.visitor.Visitor
import kotlin.getValue

data class IdentExpressionTree(val name: NameTree) : ExpressionTree {
    override val span: Span
        get() = name.span

    var references: Declaration? = null
        set(value) {
            requireNotNull(value)
            field = value
        }

    override val type by lazy {
        val declarationTree = references!!
        declarationTree.type
    }

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }
}
