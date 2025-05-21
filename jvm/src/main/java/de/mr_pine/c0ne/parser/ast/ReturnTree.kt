package de.mr_pine.c0ne.parser.ast

import edu.kit.kastel.vads.compiler.Position
import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.Span.SimpleSpan
import edu.kit.kastel.vads.compiler.parser.visitor.Visitor

@JvmRecord
data class ReturnTree(@JvmField val expression: ExpressionTree, val start: Position) : ControlTree {
    override val span: Span
        get() = SimpleSpan(this.start, this.expression.span.end)

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }
}
