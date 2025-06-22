package de.mr_pine.c0ne.lexer

import de.mr_pine.c0ne.Span

data class NumberLiteral(val value: String, val base: Int, override val span: Span) : Token {
    override fun asString() = value
}
