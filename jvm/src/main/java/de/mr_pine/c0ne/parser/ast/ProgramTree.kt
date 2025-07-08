package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.Span.SimpleSpan
import de.mr_pine.c0ne.parser.visitor.Visitor

class ProgramTree(val topLevelTrees: List<TopLevelTree>) : Tree {
    override val span: Span
        get() {
            val first = topLevelTrees.first()
            val last = topLevelTrees.last()
            return SimpleSpan(first.span.start, last.span.end)
        }

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }

    val functions
        get() = topLevelTrees.filterIsInstance<DeclaredFunctionTree>()

    init {
        assert(topLevelTrees.isNotEmpty()) { "must be non-empty" }
    }
}
