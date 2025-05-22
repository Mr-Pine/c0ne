package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.parser.visitor.Visitor

@JvmRecord
data class DeclarationTree(
    @JvmField val type: TypeTree,
    @JvmField val name: NameTree,
    @JvmField val initializer: ExpressionTree?
) : StatementTree {
    override val span: Span
        get() {
            if (this.initializer != null) {
                return this.type.span merge this.initializer.span
            }
            return this.type.span.merge(this.name.span)
        }

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }
}
