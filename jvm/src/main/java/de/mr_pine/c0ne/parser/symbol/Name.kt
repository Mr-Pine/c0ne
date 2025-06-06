package de.mr_pine.c0ne.parser.symbol

import de.mr_pine.c0ne.lexer.Identifier
import de.mr_pine.c0ne.lexer.Keyword

interface Name {
    fun asString(): String

    companion object {
        fun forKeyword(keyword: Keyword): Name {
            return KeywordName(keyword.type)
        }

        fun forIdentifier(identifier: Identifier): Name {
            return IdentName(identifier.value)
        }
    }
}
