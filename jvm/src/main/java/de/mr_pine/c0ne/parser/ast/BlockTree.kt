package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.parser.visitor.Visitor

class BlockTree(statements: List<StatementTree>, override val span: Span) : StatementTree {

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }

    val statements: List<StatementTree>

    init {
        var statements = statements
        this.statements = statements.toList()
    }
}
