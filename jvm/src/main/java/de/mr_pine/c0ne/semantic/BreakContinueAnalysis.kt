package de.mr_pine.c0ne.semantic

import de.mr_pine.c0ne.parser.ast.BreakTree
import de.mr_pine.c0ne.parser.ast.ContinueTree
import de.mr_pine.c0ne.parser.ast.ControlTree
import de.mr_pine.c0ne.parser.ast.ForTree
import de.mr_pine.c0ne.parser.ast.DeclaredFunctionTree
import de.mr_pine.c0ne.parser.ast.WhileTree
import de.mr_pine.c0ne.parser.visitor.NoOpVisitor

class BreakContinueAnalysis : NoOpVisitor<MutableList<ControlTree>> {
    override fun visit(
        breakTree: BreakTree,
        data: MutableList<ControlTree>
    ) {
        data.add(breakTree)
        super.visit(breakTree, data)
    }

    override fun visit(
        continueTree: ContinueTree,
        data: MutableList<ControlTree>
    ) {
        data.add(continueTree)
        super.visit(continueTree, data)
    }

    override fun visit(
        whileTree: WhileTree,
        data: MutableList<ControlTree>
    ) {
        data.clear()
        super.visit(whileTree, data)
    }

    override fun visit(
        forTree: ForTree,
        data: MutableList<ControlTree>
    ) {
        data.clear()
        super.visit(forTree, data)
    }

    override fun visit(
        functionTree: DeclaredFunctionTree,
        data: MutableList<ControlTree>
    ) {
        for (controlTree in data) {
            throw SemanticException("Break or continue only allowed in loops at ${controlTree.span}")
        }
        super.visit(functionTree, data)
    }
}