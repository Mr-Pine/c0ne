package edu.kit.kastel.vads.compiler.parser.ast;

import edu.kit.kastel.vads.compiler.Span;
import edu.kit.kastel.vads.compiler.parser.ParseException;
import edu.kit.kastel.vads.compiler.parser.visitor.Visitor;
import edu.kit.kastel.vads.compiler.semantic.SemanticException;
import java.util.Optional;
import java.util.OptionalLong;

public record LiteralTree(String value, int base, Span span) implements ExpressionTree {
    @Override
    public <T, R> R accept(Visitor<T, R> visitor, T data) {
        return visitor.visit(this, data);
    }

    public OptionalLong parseValue() {
        int begin = 0;
        int end = value.length();
        if (base == 16) {
            begin = 2; // ignore 0x
        }
        long l;
        try {
            l = Long.parseLong(value, begin, end, base);
        } catch (NumberFormatException _) {
            return OptionalLong.empty();
        }
        boolean isNegative = l < 0;
        boolean validPositiveInt = l <= Integer.toUnsignedLong(Integer.MIN_VALUE);
        boolean validInt = l <= Integer.toUnsignedLong(-1);
        if (isNegative || (!validPositiveInt && base != 16) || !validInt) {
            return OptionalLong.empty();
        }
        return OptionalLong.of(l);
    }

}
