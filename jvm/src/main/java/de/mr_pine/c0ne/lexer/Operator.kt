package de.mr_pine.c0ne.lexer

import edu.kit.kastel.vads.compiler.Span

@JvmRecord
data class Operator(@JvmField val type: OperatorType, override val span: Span) : Token {
    override fun isOperator(operatorType: OperatorType): Boolean {
        return this.type == operatorType
    }

    override fun asString() = type.toString()

    enum class OperatorType(private val value: String, val isSelfAssignOperator: Boolean) {
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

        override fun toString() = value
    }
}
