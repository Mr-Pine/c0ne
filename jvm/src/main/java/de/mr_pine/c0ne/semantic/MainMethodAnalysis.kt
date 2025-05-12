package de.mr_pine.c0ne.semantic

import edu.kit.kastel.vads.compiler.parser.ast.FunctionTree
import edu.kit.kastel.vads.compiler.parser.ast.ProgramTree
import edu.kit.kastel.vads.compiler.parser.visitor.NoOpVisitor
import edu.kit.kastel.vads.compiler.parser.visitor.Unit
import edu.kit.kastel.vads.compiler.semantic.SemanticException

class MainMethodAnalysis : NoOpVisitor<kotlin.Unit> {
    override fun visit(
        programTree: ProgramTree,
        data: kotlin.Unit?
    ): Unit? {
        val functions: List<FunctionTree> = programTree.topLevelTrees()
        if (functions.none { it.name.name.asString() == "main" }) {
            throw SemanticException("No main method defined")
        }
        return super.visit(programTree, data)
    }
}