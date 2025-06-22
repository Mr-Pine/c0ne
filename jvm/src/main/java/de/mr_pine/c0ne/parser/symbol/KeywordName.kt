package de.mr_pine.c0ne.parser.symbol

import de.mr_pine.c0ne.lexer.KeywordType

internal data class KeywordName(val type: KeywordType) : Name {
    override fun asString(): String {
        return type.keyword
    }
}
