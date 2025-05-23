package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.parser.visitor.Visitor

data class DeclarationTree(
    val typeDeclaration: TypeTree,
    val name: NameTree,
    val initializer: ExpressionTree?
) : StatementTree {
    override val span: Span
        get() {
            if (this.initializer != null) {
                return this.typeDeclaration.span merge this.initializer.span
            }
            return this.typeDeclaration.span.merge(this.name.span)
        }

    val type = typeDeclaration.type

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }
}
