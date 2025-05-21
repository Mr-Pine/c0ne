package edu.kit.kastel.vads.compiler.parser.visitor;

import de.mr_pine.c0ne.parser.ast.*;

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
    default Unit visit(TernaryOperationTree ternaryOperationTree, T data) {
        return Unit.INSTANCE;
    }

    @Override
    default Unit visit(LiteralTree.LiteralIntTree literalTree, T data) {
        return Unit.INSTANCE;
    }

    @Override
    default Unit visit(LiteralTree.LiteralBoolTree literalTree, T data) {
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
    default Unit visit(UnaryOperationTree unaryOperationTree, T data) {
        return Unit.INSTANCE;
    }

    @Override
    default Unit visit(ProgramTree programTree, T data) {
        return Unit.INSTANCE;
    }

    @Override
    default Unit visit(IfTree ifTree, T data) {
        return Unit.INSTANCE;
    }

    @Override
    default Unit visit(WhileTree whileTree, T data) {
        return Unit.INSTANCE;
    }

    @Override
    default Unit visit(BreakTree breakTree, T data) {
        return Unit.INSTANCE;
    }

    @Override
    default Unit visit(ContinueTree continueTree, T data) {
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
