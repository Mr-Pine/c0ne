package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.lexer.Operator
import edu.kit.kastel.vads.compiler.parser.visitor.Visitor

@JvmRecord
data class AssignmentTree(@JvmField val lValue: LValueTree, @JvmField val operator: Operator, @JvmField val expression: ExpressionTree) :
    StatementTree {
    override val span
        get() = this.lValue.span merge this.expression.span

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }
}
