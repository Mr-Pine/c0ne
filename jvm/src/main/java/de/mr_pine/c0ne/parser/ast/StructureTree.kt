package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.Span.SimpleSpan
import de.mr_pine.c0ne.parser.type.Type
import de.mr_pine.c0ne.parser.visitor.Visitor

data class StructureTree(
    val nameTree: NameTree,
    val fields: List<DeclarationTree>,
    override val span: Span,
) : TopLevelTree {

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }
}
