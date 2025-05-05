package edu.kit.kastel.vads.compiler.parser.visitor;

import edu.kit.kastel.vads.compiler.parser.ast.AssignmentTree;
import edu.kit.kastel.vads.compiler.parser.ast.BinaryOperationTree;
import edu.kit.kastel.vads.compiler.parser.ast.BlockTree;
import edu.kit.kastel.vads.compiler.parser.ast.DeclarationTree;
import edu.kit.kastel.vads.compiler.parser.ast.FunctionTree;
import edu.kit.kastel.vads.compiler.parser.ast.IdentExpressionTree;
import edu.kit.kastel.vads.compiler.parser.ast.LValueIdentTree;
import edu.kit.kastel.vads.compiler.parser.ast.LiteralTree;
import edu.kit.kastel.vads.compiler.parser.ast.NameTree;
import edu.kit.kastel.vads.compiler.parser.ast.NegateTree;
import edu.kit.kastel.vads.compiler.parser.ast.ProgramTree;
import edu.kit.kastel.vads.compiler.parser.ast.ReturnTree;
import edu.kit.kastel.vads.compiler.parser.ast.TypeTree;

/// A visitor that does nothing and returns [Unit#INSTANCE] by default.
/// This can be used to implement operations only for specific tree types.
public interface NoOpVisitor<T> extends Visitor<T, Unit> {

    @Override
    default Unit visit(AssignmentTree assignmentTree, T data) {
        return Unit.INSTANCE;
    }

    @Override
    default Unit visit(BinaryOperationTree binaryOperationTree, T data) {
        return Unit.INSTANCE;
    }

    @Override
    default Unit visit(BlockTree blockTree, T data) {
        return Unit.INSTANCE;
    }

    @Override
    default Unit visit(DeclarationTree declarationTree, T data) {
        return Unit.INSTANCE;
    }

    @Override
    default Unit visit(FunctionTree functionTree, T data) {
        return Unit.INSTANCE;
    }

    @Override
    default Unit visit(IdentExpressionTree identExpressionTree, T data) {
        return Unit.INSTANCE;
    }

    @Override
    default Unit visit(LiteralTree literalTree, T data) {
        return Unit.INSTANCE;
    }

    @Override
    default Unit visit(LValueIdentTree lValueIdentTree, T data) {
        return Unit.INSTANCE;
    }

    @Override
    default Unit visit(NameTree nameTree, T data) {
        return Unit.INSTANCE;
    }

    @Override
    default Unit visit(NegateTree negateTree, T data) {
        return Unit.INSTANCE;
    }

    @Override
    default Unit visit(ProgramTree programTree, T data) {
        return Unit.INSTANCE;
    }

    @Override
    default Unit visit(ReturnTree returnTree, T data) {
        return Unit.INSTANCE;
    }

    @Override
    default Unit visit(TypeTree typeTree, T data) {
        return Unit.INSTANCE;
    }
}
