package de.mr_pine.c0ne.parser.visitor

import de.mr_pine.c0ne.parser.ast.FunctionTree
import de.mr_pine.c0ne.parser.ast.ReturnTree
import de.mr_pine.c0ne.parser.visitor.ReturnAnalysis.ReturnState
import de.mr_pine.c0ne.semantic.SemanticException

/** Checks that functions return.
 * Currently only works for straight-line code. */
class ReturnAnalysis : NoOpVisitor<ReturnState> {
    class ReturnState {
        var returns: Boolean = false
    }

    override fun visit(returnTree: ReturnTree, data: ReturnState) {
        data.returns = true
        return super.visit(returnTree, data)
    }

    override fun visit(functionTree: FunctionTree, data: ReturnState) {
        if (!data.returns) {
            throw SemanticException("function " + functionTree.name + " does not return")
        }
        data.returns = false
        return super.visit(functionTree, data)
    }
}
