package de.mr_pine.c0ne.lexer

import de.mr_pine.c0ne.Span

data class Keyword(val type: KeywordType, override val span: Span) : Token {
    override fun isKeyword(keywordType: KeywordType): Boolean {
        return this.type == keywordType
    }

    override fun asString() = type.keyword
}
