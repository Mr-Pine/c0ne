package edu.kit.kastel.vads.compiler.lexer;

import edu.kit.kastel.vads.compiler.Span;

public record Identifier(String value, Span span) implements Token {
    @Override
    public String asString() {
        return value();
    }
}
