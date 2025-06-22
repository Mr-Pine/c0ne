package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.parser.visitor.Visitor

data class LValueIdentTree(val name: NameTree) : LValueTree {
    override val span: Span
        get() = name.span

    var references: DeclarationTree? = null
        set(value) {
            requireNotNull(value)
            field = value
        }

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }
}
