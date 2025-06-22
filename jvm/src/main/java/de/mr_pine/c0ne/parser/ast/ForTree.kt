package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.Position
import de.mr_pine.c0ne.parser.visitor.Visitor

data class ForTree(val initializer: StatementTree?, val condition: ExpressionTree, val step: StatementTree?, val loopBody: StatementTree, private val spanStart: Position) :
    ControlTree {
    override val span: Span
        get() {
            return Span.SimpleSpan(spanStart, loopBody.span.end)
        }

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }

}
