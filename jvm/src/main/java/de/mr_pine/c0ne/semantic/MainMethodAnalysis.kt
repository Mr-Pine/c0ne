package de.mr_pine.c0ne.semantic

import de.mr_pine.c0ne.parser.ast.FunctionTree
import de.mr_pine.c0ne.parser.ast.ProgramTree
import edu.kit.kastel.vads.compiler.parser.visitor.NoOpVisitor
import edu.kit.kastel.vads.compiler.parser.visitor.Unit

class MainMethodAnalysis : NoOpVisitor<kotlin.Unit> {
    override fun visit(
        programTree: ProgramTree,
        data: kotlin.Unit?
    ): Unit? {
        val functions: List<FunctionTree> = programTree.topLevelTrees
        if (functions.none { it.name.name.asString() == "main" }) {
            throw SemanticException("No main method defined")
        }
        return super.visit(programTree, data)
    }
}