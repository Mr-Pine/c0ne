package de.mr_pine.c0ne.semantic

import de.mr_pine.c0ne.parser.ast.FunctionTree
import de.mr_pine.c0ne.parser.ast.ProgramTree
import de.mr_pine.c0ne.parser.visitor.NoOpVisitor

class MainMethodAnalysis : NoOpVisitor<Unit> {
    override fun visit(
        programTree: ProgramTree,
        data: Unit
    ) {
        val functions: List<FunctionTree> = programTree.topLevelTrees
        if (functions.none { it.name.name.asString() == "main" }) {
            throw SemanticException("No main method defined")
        }
        return super.visit(programTree, data)
    }
}