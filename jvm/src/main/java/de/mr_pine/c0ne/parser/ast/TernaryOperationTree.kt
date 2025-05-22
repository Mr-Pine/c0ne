package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.parser.visitor.Visitor

data class TernaryOperationTree(val condition: ExpressionTree, val thenExpression: ExpressionTree, val elseExpression: ExpressionTree) :
    ExpressionTree {
        override val span = condition.span merge thenExpression.span merge elseExpression.span
    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }
}
