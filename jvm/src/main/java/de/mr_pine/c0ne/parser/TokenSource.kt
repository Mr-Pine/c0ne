package de.mr_pine.c0ne.parser

import de.mr_pine.c0ne.lexer.*
import de.mr_pine.c0ne.lexer.Separator.SeparatorType
import de.mr_pine.c0ne.lexer.KeywordType
import de.mr_pine.c0ne.lexer.Lexer

class TokenSource {
    private val tokens: List<Token>
    private var idx = 0

    constructor(lexer: Lexer) {
        this.tokens = generateSequence(lexer::nextToken)
            .toList()
    }

    fun peek(): Token {
        expectHasMore()
        return this.tokens[this.idx]
    }

    inline fun <reified T: Token> peekAs(): T? {
        return peek() as? T
    }

    fun expectAnyKeyword(keywords: List<KeywordType>): Keyword {
        val token = peek()
        if (token !is Keyword || token.type !in keywords) {
            throw ParseException("expected any keyword of ${keywords.joinToString()} but got $token")
        }
        this.idx++
        return token
    }

    fun expectKeyword(type: KeywordType) = expectAnyKeyword(listOf(type))

    fun expectSeparator(type: SeparatorType?): Separator {
        val token = peek()
        if (token !is Separator || token.type != type) {
            throw ParseException("expected separator '$type' but got $token")
        }
        this.idx++
        return token
    }

    fun expectOperator(type: Operator.OperatorType): Operator {
        val token = peek()
        if (token !is Operator || token.type != type) {
            throw ParseException("expected operator '$type' but got $token")
        }
        this.idx++
        return token
    }

    fun expectIdentifier(): Identifier {
        val token = peek()
        if (token !is Identifier) {
            throw ParseException("expected identifier but got $token")
        }
        this.idx++
        return token
    }

    fun consume(): Token {
        val token = peek()
        this.idx++
        return token
    }

    fun hasMore(): Boolean {
        return this.idx < this.tokens.size
    }

    private fun expectHasMore() {
        if (!hasMore()) {
            throw ParseException("reached end of file")
        }
    }
}
