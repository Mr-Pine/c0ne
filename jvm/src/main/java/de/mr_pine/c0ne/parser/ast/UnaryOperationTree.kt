package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.lexer.Operator
import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.parser.type.BasicType
import de.mr_pine.c0ne.parser.type.Type
import de.mr_pine.c0ne.parser.visitor.Visitor

data class UnaryOperationTree(val operator: Operator, val expression: ExpressionTree) : ExpressionTree {
    override val span: Span
        get() = operator.span.merge(this.expression.span)

    override val type = when (operator.type) {
        Operator.OperatorType.MINUS, Operator.OperatorType.BITWISE_NOT -> BasicType.Integer
        Operator.OperatorType.LOGICAL_NOT -> BasicType.Boolean
        else -> throw Exception("Unsupported unary operator: ${operator.type}")
    }

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }
}
