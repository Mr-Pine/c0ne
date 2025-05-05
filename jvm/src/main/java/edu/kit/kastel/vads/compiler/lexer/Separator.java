package edu.kit.kastel.vads.compiler.lexer;

import edu.kit.kastel.vads.compiler.Span;

public record Separator(SeparatorType type, Span span) implements Token {

    @Override
    public boolean isSeparator(SeparatorType separatorType) {
        return type() == separatorType;
    }

    @Override
    public String asString() {
        return type().toString();
    }

    public enum SeparatorType {
        PAREN_OPEN("("),
        PAREN_CLOSE(")"),
        BRACE_OPEN("{"),
        BRACE_CLOSE("}"),
        SEMICOLON(";");

        private final String value;

        SeparatorType(String value) {
            this.value = value;
        }

        @Override
        public String toString() {
            return this.value;
        }
    }
}
