package de.mr_pine.c0ne.lexer

import de.mr_pine.c0ne.Span

data class Separator(val type: SeparatorType, override val span: Span) : Token {
    override fun isSeparator(separatorType: SeparatorType): Boolean {
        return this.type == separatorType
    }

    override fun asString() = this.type.toString()

    enum class SeparatorType(private val value: String) {
        PAREN_OPEN("("),
        PAREN_CLOSE(")"),
        BRACE_OPEN("{"),
        BRACE_CLOSE("}"),
        SEMICOLON(";");

        override fun toString() = value
    }
}
