package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import edu.kit.kastel.vads.compiler.Position
import edu.kit.kastel.vads.compiler.parser.visitor.Visitor

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
