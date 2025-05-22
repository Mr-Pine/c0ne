package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.Span.SimpleSpan
import de.mr_pine.c0ne.parser.visitor.Visitor

@JvmRecord
data class FunctionTree(
    @JvmField val returnType: TypeTree,
    @JvmField val name: NameTree,
    @JvmField val body: BlockTree
) : Tree {
    override val span: Span
        get() = SimpleSpan(this.returnType.span.start, this.body.span.end)

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }
}
