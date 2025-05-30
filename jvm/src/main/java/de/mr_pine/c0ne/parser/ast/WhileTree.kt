package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.Position
import de.mr_pine.c0ne.parser.visitor.Visitor

data class WhileTree(val condition: ExpressionTree, val loopBody: StatementTree, private val spanStart: Position) :
    ControlTree {
    override val span: Span
        get() {
            return Span.SimpleSpan(spanStart, (condition.span merge loopBody.span).end)
        }

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }

}
