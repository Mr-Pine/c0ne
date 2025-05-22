package de.mr_pine.c0ne.semantic

import de.mr_pine.c0ne.parser.ast.LiteralTree
import de.mr_pine.c0ne.parser.visitor.NoOpVisitor

class IntegerLiteralRangeAnalysis : NoOpVisitor<Unit> {
    override fun visit(literalIntTree: LiteralTree.LiteralIntTree, data: Unit) {
        literalIntTree.parseValue() ?: throw SemanticException("invalid integer literal " + literalIntTree.value)
        return super.visit(literalIntTree, data)
    }
}
