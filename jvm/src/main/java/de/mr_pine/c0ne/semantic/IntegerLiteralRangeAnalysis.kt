package de.mr_pine.c0ne.semantic

import de.mr_pine.c0ne.parser.ast.LiteralTree
import edu.kit.kastel.vads.compiler.parser.visitor.NoOpVisitor
import edu.kit.kastel.vads.compiler.parser.visitor.Unit
import edu.kit.kastel.vads.compiler.semantic.Namespace
import edu.kit.kastel.vads.compiler.semantic.SemanticException

class IntegerLiteralRangeAnalysis : NoOpVisitor<Namespace<Void?>> {
    override fun visit(literalIntTree: LiteralTree.LiteralIntTree, data: Namespace<Void?>): Unit? {
        literalIntTree.parseValue() ?: throw SemanticException("invalid integer literal " + literalIntTree.value)
        return super.visit(literalIntTree, data)
    }
}
