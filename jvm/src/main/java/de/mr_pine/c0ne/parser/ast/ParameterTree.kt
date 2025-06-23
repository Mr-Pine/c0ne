package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.parser.visitor.Visitor

data class ParameterTree(
    override val typeDeclaration: TypeTree,
    override val name: NameTree
) : Declaration {
    override val span: Span
        get() {
            return this.typeDeclaration.span.merge(this.name.span)
        }

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }
}
