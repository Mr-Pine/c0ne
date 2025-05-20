package de.mr_pine.c0ne.lexer

import edu.kit.kastel.vads.compiler.Span

data class NumberLiteral(val value: String, val base: Int, override val span: Span) : Token {
    override fun asString() = value
}
