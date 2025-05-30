package de.mr_pine.c0ne.semantic

import de.mr_pine.c0ne.parser.ast.DeclarationTree
import de.mr_pine.c0ne.parser.ast.ForTree
import de.mr_pine.c0ne.parser.visitor.NoOpVisitor

/** Checks that functions return.
 * Currently only works for straight-line code. */
class ForStepAnalysis : NoOpVisitor<Unit> {
    override fun visit(forTree: ForTree, data: Unit) {
        if (forTree.step is DeclarationTree) {
            throw SemanticException("for loop step must not be a declaration at ${forTree.span}")
        }
        super.visit(forTree, data)
    }
}