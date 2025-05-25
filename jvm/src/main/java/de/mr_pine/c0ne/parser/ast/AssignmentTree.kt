package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.lexer.Operator
import de.mr_pine.c0ne.parser.visitor.Visitor

data class AssignmentTree(val lValue: LValueTree, val operator: Operator, val expression: ExpressionTree) :
    StatementTree {
    override val span
        get() = this.lValue.span merge this.expression.span

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }
}
