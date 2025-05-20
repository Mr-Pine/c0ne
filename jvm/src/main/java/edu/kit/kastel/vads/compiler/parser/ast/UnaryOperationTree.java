package edu.kit.kastel.vads.compiler.parser.ast;

import de.mr_pine.c0ne.lexer.Operator;
import edu.kit.kastel.vads.compiler.Span;
import edu.kit.kastel.vads.compiler.parser.visitor.Visitor;

public record UnaryOperationTree(Operator operator, ExpressionTree expression) implements ExpressionTree {
    @Override
    public Span span() {
        return operator.span().merge(expression().span());
    }

    @Override
    public <T, R> R accept(Visitor<T, R> visitor, T data) {
        return visitor.visit(this, data);
    }
}
