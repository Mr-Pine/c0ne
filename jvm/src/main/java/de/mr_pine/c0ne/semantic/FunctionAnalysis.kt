package de.mr_pine.c0ne.semantic

import de.mr_pine.c0ne.parser.ast.CallTree
import de.mr_pine.c0ne.parser.ast.FunctionTree
import de.mr_pine.c0ne.parser.ast.ProgramTree
import de.mr_pine.c0ne.parser.symbol.IdentName
import de.mr_pine.c0ne.parser.type.BasicType
import de.mr_pine.c0ne.parser.visitor.NoOpVisitor

class FunctionAnalysis(program: ProgramTree) : NoOpVisitor<Unit> {
    val builtins = listOf(
        FunctionTree.BuiltinFunction(IdentName("print"), BasicType.Integer, listOf(BasicType.Integer)),
        FunctionTree.BuiltinFunction(IdentName("read"), BasicType.Integer, listOf()),
        FunctionTree.BuiltinFunction(IdentName("flush"), BasicType.Integer, listOf()),
    )

    var functions: List<FunctionTree> = program.functions + builtins

    init {
        for ((name, definitions) in functions.groupBy { it.name }) {
            if (definitions.size > 1) throw SemanticException("Duplicate function definition for $name")
        }
    }

    override fun visit(callTree: CallTree, data: Unit) {
        val name = callTree.identifier.name
        val function = functions.find { it.name == name }
            ?: throw SemanticException("Unknown function $name at ${callTree.span} call")
        if (function.parameterTypes.size != callTree.arguments.elements.size) {
            throw SemanticException("Wrong number of arguments for function $name at ${callTree.span} call. Expected ${function.parameterTypes.size} got ${callTree.arguments.elements.size}")
        }
        callTree.references = function
        super.visit(callTree, data)
    }
}