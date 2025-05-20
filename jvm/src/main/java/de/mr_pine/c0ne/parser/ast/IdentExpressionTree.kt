package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import edu.kit.kastel.vads.compiler.parser.visitor.Visitor

@JvmRecord
data class IdentExpressionTree(@JvmField val name: NameTree) : ExpressionTree {
    override val span: Span
        get() = name.span

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }
}
