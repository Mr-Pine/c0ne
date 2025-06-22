package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Position
import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.Span.SimpleSpan
import de.mr_pine.c0ne.parser.visitor.Visitor

data class CallTree(
    val identifier: NameTree,
    val arguments: ParenthesizedListTree<ExpressionTree>
) : Tree, StatementTree, ExpressionTree {
    override val span = this.identifier.span merge arguments.span

    var references: FunctionTree? = null
        set(value) {
            requireNotNull(value)
            field = value
        }

    override val type by lazy {
        val declarationTree = references!!
        declarationTree.returnType
    }

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }
}
