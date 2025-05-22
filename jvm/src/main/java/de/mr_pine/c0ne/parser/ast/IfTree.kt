package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import edu.kit.kastel.vads.compiler.Position
import de.mr_pine.c0ne.parser.visitor.Visitor

data class IfTree(val condition: ExpressionTree, val thenTree: StatementTree, val elseTree: StatementTree?, private val spanStart: Position) :
    ControlTree {
    override val span: Span
        get() {
            val baseSpan = condition.span merge thenTree.span
            val endSpan = if (elseTree != null) {
                return baseSpan merge elseTree.span
            } else {
                baseSpan
            }
            return Span.SimpleSpan(spanStart, endSpan.end)
        }

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }

}
