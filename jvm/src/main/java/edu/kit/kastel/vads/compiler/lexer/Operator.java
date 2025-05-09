package edu.kit.kastel.vads.compiler.lexer;

import edu.kit.kastel.vads.compiler.Span;

public record Operator(OperatorType type, Span span) implements Token {

    @Override
    public boolean isOperator(OperatorType operatorType) {
        return type() == operatorType;
    }

    @Override
    public String asString() {
        return type().toString();
    }

    public enum OperatorType {
        ASSIGN_MINUS("-=", true),
        MINUS("-", false),
        ASSIGN_PLUS("+=", true),
        PLUS("+", false),
        MUL("*", false),
        ASSIGN_MUL("*=", true),
        ASSIGN_DIV("/=", true),
        DIV("/", false),
        ASSIGN_MOD("%=", true),
        MOD("%", false),
        ASSIGN("=", false),
        ;

        private final String value;
        private final boolean selfAssignOperator;

        OperatorType(String value, boolean selfAssignOperator) {
            this.value = value;
            this.selfAssignOperator = selfAssignOperator;
        }

        @Override
        public String toString() {
            return this.value;
        }

        public boolean isSelfAssignOperator() {
            return selfAssignOperator;
        }
    }
}
