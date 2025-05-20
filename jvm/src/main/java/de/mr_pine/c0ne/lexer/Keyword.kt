package de.mr_pine.c0ne.lexer

import de.mr_pine.c0ne.Span
import edu.kit.kastel.vads.compiler.lexer.KeywordType

@JvmRecord
data class Keyword(@JvmField val type: KeywordType, override val span: Span) : Token {
    override fun isKeyword(keywordType: KeywordType): Boolean {
        return this.type == keywordType
    }

    override fun asString() = type.keyword()!!
}
