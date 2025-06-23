package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.Span.SimpleSpan
import de.mr_pine.c0ne.parser.visitor.Visitor

data class DeclaredFunctionTree(
    val returnTypeTree: TypeTree,
    val name: NameTree,
    val parameters: ParenthesizedListTree<ParameterTree>,
    val body: BlockTree
) : FunctionTree {
    override val span: Span
        get() = SimpleSpan(this.returnTypeTree.span.start, this.body.span.end)

    override val returnType
        get() = returnTypeTree.type

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }
}
