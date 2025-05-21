package edu.kit.kastel.vads.compiler.parser.visitor;

import de.mr_pine.c0ne.parser.ast.*;

/// A visitor that traverses a tree in postorder
///
/// @param <T> a type for additional data
/// @param <R> a type for a return type
public class RecursivePostorderVisitor<T, R> implements Visitor<T, R> {
    private final Visitor<T, R> visitor;

    public RecursivePostorderVisitor(Visitor<T, R> visitor) {
        this.visitor = visitor;
    }

    @Override
    public R visit(AssignmentTree assignmentTree, T data) {
        R r = assignmentTree.lValue.accept(this, data);
        r = assignmentTree.expression.accept(this, accumulate(data, r));
        r = this.visitor.visit(assignmentTree, accumulate(data, r));
        return r;
    }

    @Override
    public R visit(BinaryOperationTree binaryOperationTree, T data) {
        R r = binaryOperationTree.getLhs().accept(this, data);
        r = binaryOperationTree.getRhs().accept(this, accumulate(data, r));
        r = this.visitor.visit(binaryOperationTree, accumulate(data, r));
        return r;
    }

    @Override
    public R visit(BlockTree blockTree, T data) {
        R r;
        T d = data;
        for (StatementTree statement : blockTree.getStatements()) {
            r = statement.accept(this, d);
            d = accumulate(d, r);
        }
        r = this.visitor.visit(blockTree, d);
        return r;
    }

    @Override
    public R visit(DeclarationTree declarationTree, T data) {
        R r = declarationTree.type.accept(this, data);
        r = declarationTree.name.accept(this, accumulate(data, r));
        if (declarationTree.initializer != null) {
            r = declarationTree.initializer.accept(this, accumulate(data, r));
        }
        r = this.visitor.visit(declarationTree, accumulate(data, r));
        return r;
    }

    @Override
    public R visit(FunctionTree functionTree, T data) {
        R r = functionTree.returnType.accept(this, data);
        r = functionTree.name.accept(this, accumulate(data, r));
        r = functionTree.body.accept(this, accumulate(data, r));
        r = this.visitor.visit(functionTree, accumulate(data, r));
        return r;
    }

    @Override
    public R visit(TernaryOperationTree ternaryOperationTree, T data) {
        R r = ternaryOperationTree.getCondition().accept(this, data);
        r = ternaryOperationTree.getThenExpression().accept(this, accumulate(data, r));
        r = ternaryOperationTree.getElseExpression().accept(this, accumulate(data, r));
        return this.visitor.visit(ternaryOperationTree, accumulate(data, r));
    }

    @Override
    public R visit(IdentExpressionTree identExpressionTree, T data) {
        R r = identExpressionTree.name.accept(this, data);
        r = this.visitor.visit(identExpressionTree, accumulate(data, r));
        return r;
    }

    @Override
    public R visit(LiteralTree.LiteralIntTree literalIntTree, T data) {
        return this.visitor.visit(literalIntTree, data);
    }

    @Override
    public R visit(LiteralTree.LiteralBoolTree literalBoolTree, T data) {
        return this.visitor.visit(literalBoolTree, data);
    }

    @Override
    public R visit(LValueIdentTree lValueIdentTree, T data) {
        R r = lValueIdentTree.name.accept(this, data);
        r = this.visitor.visit(lValueIdentTree, accumulate(data, r));
        return r;
    }

    @Override
    public R visit(NameTree nameTree, T data) {
        return this.visitor.visit(nameTree, data);
    }

    @Override
    public R visit(UnaryOperationTree unaryOperationTree, T data) {
        R r = unaryOperationTree.expression.accept(this, data);
        r = this.visitor.visit(unaryOperationTree, accumulate(data, r));
        return r;
    }

    @Override
    public R visit(ProgramTree programTree, T data) {
        R r;
        T d = data;
        for (FunctionTree tree : programTree.getTopLevelTrees()) {
            r = tree.accept(this, d);
            d = accumulate(data, r);
        }
        r = this.visitor.visit(programTree, d);
        return r;
    }

    @Override
    public R visit(IfTree ifTree, T data) {
        R r = ifTree.getCondition().accept(this, data);
        r = ifTree.getThenTree().accept(this, accumulate(data, r));
        if (ifTree.getElseTree() != null) {
            r = ifTree.getThenTree().accept(this, accumulate(data, r));
        }
        r = this.visitor.visit(ifTree, accumulate(data, r));
        return r;
    }

    @Override
    public R visit(WhileTree whileTree, T data) {
        R r = whileTree.getCondition().accept(this, data);
        r = whileTree.getLoopBody().accept(this, accumulate(data, r));
        r = this.visitor.visit(whileTree, accumulate(data, r));
        return r;
    }

    @Override
    public R visit(ForTree forTree, T data) {
        R r;
        if (forTree.getInitializer() != null) {
            r = forTree.getInitializer().accept(this, data);
            r = forTree.getCondition().accept(this, accumulate(data, r));
        } else {
            r = forTree.getCondition().accept(this, data);
        }
        if (forTree.getStep() != null) {
            r = forTree.getStep().accept(this, accumulate(data, r));
        }
        r = forTree.getLoopBody().accept(this, accumulate(data, r));
        r = this.visitor.visit(forTree, accumulate(data, r));
        return r;
    }

    @Override
    public R visit(BreakTree breakTree, T data) {
        return this.visitor.visit(breakTree, data);
    }

    @Override
    public R visit(ContinueTree continueTree, T data) {
        return this.visitor.visit(continueTree, data);
    }

    @Override
    public R visit(ReturnTree returnTree, T data) {
        R r = returnTree.expression.accept(this, data);
        r = this.visitor.visit(returnTree, accumulate(data, r));
        return r;
    }

    @Override
    public R visit(TypeTree typeTree, T data) {
        return this.visitor.visit(typeTree, data);
    }

    protected T accumulate(T data, R value) {
        return data;
    }
}
