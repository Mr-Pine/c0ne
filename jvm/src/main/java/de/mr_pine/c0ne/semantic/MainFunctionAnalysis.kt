package de.mr_pine.c0ne.semantic

import de.mr_pine.c0ne.parser.ast.DeclaredFunctionTree
import de.mr_pine.c0ne.parser.ast.ProgramTree
import de.mr_pine.c0ne.parser.type.BasicType
import de.mr_pine.c0ne.parser.visitor.NoOpVisitor

class MainFunctionAnalysis : NoOpVisitor<Unit> {
    override fun visit(
        programTree: ProgramTree,
        data: Unit
    ) {
        val functions: List<DeclaredFunctionTree> = programTree.functions
        val mainFunction = functions.find { it.nameTree.name.asString() == "main" }
        if (mainFunction == null) {
            throw SemanticException("No main method defined")
        }
        if (mainFunction.returnType != BasicType.Integer) {
            throw SemanticException("main method must return integer")
        }
        if (mainFunction.parameterTypes.isNotEmpty()) {
            throw SemanticException("main method must not have parameters")
        }
        return super.visit(programTree, data)
    }
}