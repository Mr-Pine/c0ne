package de.mr_pine.c0ne.semantic

import de.mr_pine.c0ne.parser.ast.DeclaredFunctionTree
import de.mr_pine.c0ne.parser.ast.ProgramTree
import de.mr_pine.c0ne.parser.type.BasicType
import de.mr_pine.c0ne.parser.visitor.NoOpVisitor

class MainMethodAnalysis : NoOpVisitor<Unit> {
    override fun visit(
        programTree: ProgramTree,
        data: Unit
    ) {
        val functions: List<DeclaredFunctionTree> = programTree.topLevelTrees
        val mainFunction = functions.find { it.name.name.asString() == "main" }
        if (mainFunction == null) {
            throw SemanticException("No main method defined")
        }
        if (mainFunction.returnType != BasicType.Integer) {
            throw SemanticException("main method must return integer")
        }
        return super.visit(programTree, data)
    }
}