package de.mr_pine.c0ne.lexer

import edu.kit.kastel.vads.compiler.Span

@JvmRecord
data class ErrorToken(val value: String, override val span: Span) : Token {
    override fun asString() = value
}
