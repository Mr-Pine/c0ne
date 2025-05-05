package edu.kit.kastel.vads.compiler.parser.ast;

import edu.kit.kastel.vads.compiler.Span;
import edu.kit.kastel.vads.compiler.parser.visitor.Visitor;
import org.jspecify.annotations.Nullable;

public record DeclarationTree(TypeTree type, NameTree name, @Nullable ExpressionTree initializer) implements StatementTree {
    @Override
    public Span span() {
        if (initializer() != null) {
            return type().span().merge(initializer().span());
        }
        return type().span().merge(name().span());
    }

    @Override
    public <T, R> R accept(Visitor<T, R> visitor, T data) {
        return visitor.visit(this, data);
    }
}
