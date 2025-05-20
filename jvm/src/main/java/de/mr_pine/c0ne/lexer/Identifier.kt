package de.mr_pine.c0ne.lexer

import de.mr_pine.c0ne.Span

@JvmRecord
data class Identifier(@JvmField val value: String, override val span: Span) : Token {
    override fun asString() = value
}
