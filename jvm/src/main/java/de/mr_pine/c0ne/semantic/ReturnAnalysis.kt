package de.mr_pine.c0ne.semantic

import de.mr_pine.c0ne.parser.ast.BlockTree
import de.mr_pine.c0ne.parser.ast.FunctionTree
import de.mr_pine.c0ne.parser.ast.IfTree
import de.mr_pine.c0ne.parser.ast.ReturnTree
import de.mr_pine.c0ne.parser.ast.StatementTree
import de.mr_pine.c0ne.parser.visitor.NoOpVisitor

/** Checks that functions return.
 * Currently only works for straight-line code. */
class ReturnAnalysis : NoOpVisitor<ReturnAnalysis.ReturnState> {
    class ReturnState {
        val returningStatements = mutableSetOf<StatementTree>()
    }

    override fun visit(returnTree: ReturnTree, data: ReturnState) {
        data.returningStatements.add(returnTree)
        return super.visit(returnTree, data)
    }

    override fun visit(
        blockTree: BlockTree,
        data: ReturnState
    ) {
        if (data.returningStatements.intersect(blockTree.statements).isNotEmpty()) {
            data.returningStatements.add(blockTree)
        }
        super.visit(blockTree, data)
    }

    override fun visit(
        ifTree: IfTree,
        data: ReturnState
    ) {
        if (ifTree.thenTree in data.returningStatements && ifTree.elseTree in data.returningStatements) {
            data.returningStatements.add(ifTree)
        }
        super.visit(ifTree, data)
    }

    override fun visit(functionTree: FunctionTree, data: ReturnState) {
        val body = functionTree.body
        if (body !in data.returningStatements) {
            throw SemanticException("function " + functionTree.name + " does not return")
        }
        data.returningStatements.clear()
        return super.visit(functionTree, data)
    }
}