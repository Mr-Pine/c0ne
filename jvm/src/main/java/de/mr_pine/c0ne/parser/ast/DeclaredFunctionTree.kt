package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.Span.SimpleSpan
import de.mr_pine.c0ne.parser.type.Type
import de.mr_pine.c0ne.parser.visitor.Visitor

data class DeclaredFunctionTree(
    val returnTypeTree: TypeTree,
    val nameTree: NameTree,
    val parameters: ParenthesizedListTree<ParameterTree>,
    val body: BlockTree
) : FunctionTree, TopLevelTree {
    override val span: Span
        get() = SimpleSpan(this.returnTypeTree.span.start, this.body.span.end)

    override val name = nameTree.name

    override val returnType
        get() = returnTypeTree.type
    override val parameterTypes: List<Type>
        get() = parameters.elements.map { it.type }

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }
}
