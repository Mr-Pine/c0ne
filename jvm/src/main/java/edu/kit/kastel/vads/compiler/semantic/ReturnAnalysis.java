package edu.kit.kastel.vads.compiler.semantic;

import edu.kit.kastel.vads.compiler.parser.ast.FunctionTree;
import edu.kit.kastel.vads.compiler.parser.ast.ReturnTree;
import edu.kit.kastel.vads.compiler.parser.visitor.NoOpVisitor;
import edu.kit.kastel.vads.compiler.parser.visitor.Unit;

/// Checks that functions return.
/// Currently only works for straight-line code.
class ReturnAnalysis implements NoOpVisitor<ReturnAnalysis.ReturnState> {

    static class ReturnState {
        boolean returns = false;
    }

    @Override
    public Unit visit(ReturnTree returnTree, ReturnState data) {
        data.returns = true;
        return NoOpVisitor.super.visit(returnTree, data);
    }

    @Override
    public Unit visit(FunctionTree functionTree, ReturnState data) {
        if (!data.returns) {
            throw new SemanticException("function " + functionTree.name() + " does not return");
        }
        data.returns = false;
        return NoOpVisitor.super.visit(functionTree, data);
    }
}
