package edu.kit.kastel.vads.compiler.semantic;

import de.mr_pine.c0ne.parser.ast.AssignmentTree;
import de.mr_pine.c0ne.parser.ast.DeclarationTree;
import de.mr_pine.c0ne.parser.ast.IdentExpressionTree;
import de.mr_pine.c0ne.parser.ast.LValueIdentTree;
import de.mr_pine.c0ne.parser.ast.NameTree;
import edu.kit.kastel.vads.compiler.parser.visitor.NoOpVisitor;
import edu.kit.kastel.vads.compiler.parser.visitor.Unit;
import org.jspecify.annotations.Nullable;

import java.util.Locale;

/// Checks that variables are
/// - declared before assignment
/// - not declared twice
/// - not initialized twice
/// - assigned before referenced
class VariableStatusAnalysis implements NoOpVisitor<Namespace<VariableStatusAnalysis.VariableStatus>> {

    @Override
    public Unit visit(AssignmentTree assignmentTree, Namespace<VariableStatus> data) {
        switch (assignmentTree.lValue) {
            case LValueIdentTree lValueIdentTree -> {
                VariableStatus status = data.get(lValueIdentTree.name);
                if (assignmentTree.operator.type.isSelfAssignOperator()) {
                    checkInitialized(lValueIdentTree.name, status);
                } else {
                    checkDeclared(lValueIdentTree.name, status);
                }
                data.put(lValueIdentTree.name, VariableStatus.INITIALIZED, (_, replacement) -> replacement);
            }
            default -> throw new IllegalStateException("Unexpected value: " + assignmentTree.lValue);
        }
        return NoOpVisitor.super.visit(assignmentTree, data);
    }

    private static void checkDeclared(NameTree name, @Nullable VariableStatus status) {
        if (status == null) {
            throw new SemanticException("Variable " + name + " must be declared before assignment");
        }
    }

    private static void checkInitialized(NameTree name, @Nullable VariableStatus status) {
        if (status != VariableStatus.INITIALIZED) {
            throw new SemanticException("Variable " + name + " must be initialized before usage");
        }
    }

    @Override
    public Unit visit(DeclarationTree declarationTree, Namespace<VariableStatus> data) {
        VariableStatus status = declarationTree.initializer == null
                ? VariableStatus.DECLARED
                : VariableStatus.INITIALIZED;
        data.put(declarationTree.name, status, (existing, replacement) -> {
            throw new SemanticException("variable is already " + existing + ". Cannot be " + replacement + " here.");
        });
        return NoOpVisitor.super.visit(declarationTree, data);
    }

    @Override
    public Unit visit(IdentExpressionTree identExpressionTree, Namespace<VariableStatus> data) {
        VariableStatus status = data.get(identExpressionTree.name);
        checkInitialized(identExpressionTree.name, status);
        return NoOpVisitor.super.visit(identExpressionTree, data);
    }

    enum VariableStatus {
        DECLARED,
        INITIALIZED;

        @Override
        public String toString() {
            return name().toLowerCase(Locale.ROOT);
        }
    }
}
