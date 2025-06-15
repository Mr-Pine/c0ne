package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.Span.SimpleSpan
import de.mr_pine.c0ne.parser.visitor.Visitor

class ProgramTree(topLevelTrees: List<FunctionTree>) : Tree {
    override val span: Span
        get() {
            val first: FunctionTree = topLevelTrees.first()
            val last: FunctionTree = topLevelTrees.last()
            return SimpleSpan(first.span.start, last.span.end)
        }

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }

    val topLevelTrees: List<FunctionTree>

    init {
        var topLevelTrees = topLevelTrees
        assert(topLevelTrees.isNotEmpty()) { "must be non-empty" }
        this.topLevelTrees = topLevelTrees.toList()
    }
}
