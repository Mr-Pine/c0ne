package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.lexer.Operator
import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.parser.visitor.Visitor

@JvmRecord
data class UnaryOperationTree(val operator: Operator, @JvmField val expression: ExpressionTree) : ExpressionTree {
    override val span: Span
        get() = operator.span.merge(this.expression.span)

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }
}
