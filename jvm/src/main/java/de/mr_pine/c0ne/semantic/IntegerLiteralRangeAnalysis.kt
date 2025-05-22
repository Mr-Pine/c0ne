package de.mr_pine.c0ne.semantic

import de.mr_pine.c0ne.parser.ast.LiteralTree
import edu.kit.kastel.vads.compiler.parser.visitor.NoOpVisitor
import edu.kit.kastel.vads.compiler.semantic.Namespace

class IntegerLiteralRangeAnalysis : NoOpVisitor<Unit> {
    override fun visit(literalIntTree: LiteralTree.LiteralIntTree, data: Unit) {
        literalIntTree.parseValue() ?: throw SemanticException("invalid integer literal " + literalIntTree.value)
        return super.visit(literalIntTree, data)
    }
}
