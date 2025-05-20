package de.mr_pine.c0ne.lexer

import de.mr_pine.c0ne.Span

@JvmRecord
data class Operator(@JvmField val type: OperatorType, override val span: Span) : Token {
    override fun isOperator(operatorType: OperatorType): Boolean {
        return this.type == operatorType
    }

    override fun asString() = type.toString()

    enum class OperatorType(private val value: String, val precedences: List<Int>, val isSelfAssignOperator: Boolean) {
        LOGICAL_NOT("!", listOf(1), true),
        BITWISE_NOT("~", listOf(1), true),
        MINUS("-", listOf(1, 3), false),

        MUL("*", listOf(2), false),
        DIV("/", listOf(2), false),
        MOD("%", listOf(2), false),

        PLUS("+", listOf(3), false),

        LEFT_SHIFT("<<", listOf(4), false),
        RIGHT_SHIFT(">>", listOf(4), false),

        LESS_THAN("<", listOf(5), false),
        LESS_THAN_OR_EQUAL("<=", listOf(5), false),
        GREATER_THAN(">", listOf(5), false),
        GREATER_THAN_OR_EQUAL(">=", listOf(5), false),
        EQUALS("==", listOf(5), false),
        NOT_EQUALS("!=", listOf(5), false),

        BITWISE_AND("&", listOf(6), false),
        BITWISE_XOR("^", listOf(7), false),
        BITWISE_OR("|", listOf(8), false),

        LOGICAL_AND("&&", listOf(9), false),

        LOGICAL_OR("||", listOf(10), false),

        TERNARY_QUESTION("?", listOf(), false),
        TERNARY_COLON(":", listOf(), false),

        ASSIGN("=", listOf(), false),
        ASSIGN_PLUS("+=", listOf(), true),
        ASSIGN_MINUS("-=", listOf(), true),
        ASSIGN_MUL("*=", listOf(), true),
        ASSIGN_DIV("/=", listOf(), true),
        ASSIGN_MOD("%=", listOf(), true),
        ASSIGN_AND("&=", listOf(), true),
        ASSIGN_XOR("^=", listOf(), true),
        ASSIGN_OR("|=", listOf(), true),
        ASSIGN_LEFT_SHIFT("<<=", listOf(), true),
        ASSIGN_RIGHT_SHIFT(">>=", listOf(), true),
        ;

        companion object {
            const val UNARY_PRECEDENCE_LEVEL = 1
            val MAX_PRECEDENCE = entries.maxOf { it.precedences.maxOrNull() ?: 0 }
        }

        override fun toString() = value
    }
}
