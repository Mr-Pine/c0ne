package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.lexer.Operator
import de.mr_pine.c0ne.Span
import edu.kit.kastel.vads.compiler.parser.visitor.Visitor

data class BinaryOperationTree(
    val lhs: ExpressionTree, val rhs: ExpressionTree, val operatorType: Operator.OperatorType
) : ExpressionTree {
    override val span = this.lhs.span merge this.rhs.span

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }
}
