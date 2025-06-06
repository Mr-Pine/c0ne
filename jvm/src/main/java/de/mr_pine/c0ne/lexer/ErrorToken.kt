package de.mr_pine.c0ne.lexer

import de.mr_pine.c0ne.Span

data class ErrorToken(val value: String, override val span: Span) : Token {
    override fun asString() = value
}
