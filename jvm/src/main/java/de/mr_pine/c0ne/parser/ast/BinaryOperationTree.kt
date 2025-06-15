package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.lexer.Operator
import de.mr_pine.c0ne.parser.type.BasicType
import de.mr_pine.c0ne.parser.visitor.Visitor
import de.mr_pine.c0ne.semantic.outType

data class BinaryOperationTree(
    val lhs: ExpressionTree, val rhs: ExpressionTree, val operatorType: Operator.OperatorType
) : ExpressionTree {
    override val span = this.lhs.span merge this.rhs.span

    override val type = operatorType.outType

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }
}
