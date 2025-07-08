package de.mr_pine.c0ne.lexer

import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.Span.SimpleSpan
import de.mr_pine.c0ne.lexer.Separator.SeparatorType
import de.mr_pine.c0ne.Position.SimplePosition

class Lexer private constructor(private val source: String) {
    private var pos = 0
    private var lineStart = 0
    private var line = 0

    fun nextToken(): Token? {
        val error = skipWhitespace()
        if (error != null) {
            return error
        }
        if (this.pos >= this.source.length) {
            return null
        }
        val t = when (peek()) {
            '(' -> separator(SeparatorType.PAREN_OPEN)
            ')' -> separator(SeparatorType.PAREN_CLOSE)
            '{' -> separator(SeparatorType.BRACE_OPEN)
            '}' -> separator(SeparatorType.BRACE_CLOSE)
            '[' -> separator(SeparatorType.BRACKET_OPEN)
            ']' -> separator(SeparatorType.BRACKET_CLOSE)
            ';' -> separator(SeparatorType.SEMICOLON)
            ',' -> separator(SeparatorType.COMMA)
            '-' -> singleOrWithEquals(Operator.OperatorType.MINUS, Operator.OperatorType.ASSIGN_MINUS)
            '+' -> singleOrWithEquals(Operator.OperatorType.PLUS, Operator.OperatorType.ASSIGN_PLUS)
            '*' -> singleOrWithEquals(Operator.OperatorType.MUL, Operator.OperatorType.ASSIGN_MUL)
            '/' -> singleOrWithEquals(Operator.OperatorType.DIV, Operator.OperatorType.ASSIGN_DIV)
            '%' -> singleOrWithEquals(Operator.OperatorType.MOD, Operator.OperatorType.ASSIGN_MOD)
            '!' -> singleOrWithEquals(Operator.OperatorType.LOGICAL_NOT, Operator.OperatorType.NOT_EQUALS)
            '=' -> singleOrWithEquals(Operator.OperatorType.ASSIGN, Operator.OperatorType.EQUALS)
            '&' -> singleOrDoubleOrWithEquals(
                '&',
                Operator.OperatorType.BITWISE_AND,
                Operator.OperatorType.LOGICAL_AND,
                Operator.OperatorType.ASSIGN_AND
            )

            '|' -> singleOrDoubleOrWithEquals(
                '|',
                Operator.OperatorType.BITWISE_OR,
                Operator.OperatorType.LOGICAL_OR,
                Operator.OperatorType.ASSIGN_OR
            )

            '^' -> singleOrDoubleOrWithEquals(
                '^',
                Operator.OperatorType.BITWISE_XOR,
                Operator.OperatorType.ASSIGN_XOR,
                Operator.OperatorType.ASSIGN_XOR
            )

            '<' -> if (hasMore(1) && peek(1) == '=') {
                Operator(Operator.OperatorType.LESS_THAN_OR_EQUAL, buildSpan(2))
            } else if (hasMore(2) && peek(1) == '<' && peek(2) == '=') {
                Operator(Operator.OperatorType.ASSIGN_LEFT_SHIFT, buildSpan(3))
            } else if (hasMore(1) && peek(1) == '<') {
                Operator(Operator.OperatorType.LEFT_SHIFT, buildSpan(2))
            } else {
                Operator(Operator.OperatorType.LESS_THAN, buildSpan(1))
            }

            '>' -> if (hasMore(1) && peek(1) == '=') {
                Operator(Operator.OperatorType.GREATER_THAN_OR_EQUAL, buildSpan(2))
            } else if (hasMore(2) && peek(1) == '>' && peek(2) == '=') {
                Operator(Operator.OperatorType.ASSIGN_RIGHT_SHIFT, buildSpan(3))
            } else if (hasMore(1) && peek(1) == '>') {
                Operator(Operator.OperatorType.RIGHT_SHIFT, buildSpan(2))
            } else {
                Operator(Operator.OperatorType.GREATER_THAN, buildSpan(1))
            }

            '~' -> Operator(Operator.OperatorType.BITWISE_NOT, buildSpan(1))
            '?' -> Operator(Operator.OperatorType.TERNARY_QUESTION, buildSpan(1))
            ':' -> Operator(Operator.OperatorType.TERNARY_COLON, buildSpan(1))
            else -> {
                if (isIdentifierChar(peek())) {
                    if (isNumeric(peek())) {
                        lexNumber()
                    } else {
                        lexIdentifierOrKeyword()
                    }
                } else {
                    ErrorToken(peek().toString(), buildSpan(1))
                }
            }
        }

        return t
    }

    private enum class CommentType {
        SINGLE_LINE,
        MULTI_LINE
    }

    private fun skipWhitespace(): ErrorToken? {

        var currentCommentType: CommentType? = null
        var multiLineCommentDepth = 0
        var commentStart = -1
        while (hasMore(0)) {
            when (peek()) {
                ' ', '\t' -> this.pos++
                '\n', '\r' -> {
                    this.pos++
                    this.lineStart = this.pos
                    this.line++
                    if (currentCommentType == CommentType.SINGLE_LINE) {
                        currentCommentType = null
                    }
                }

                '/' -> {
                    if (currentCommentType == CommentType.SINGLE_LINE) {
                        this.pos++
                        continue
                    }
                    if (hasMore(1)) {
                        if (peek(1) == '/' && currentCommentType == null) {
                            currentCommentType = CommentType.SINGLE_LINE
                        } else if (peek(1) == '*') {
                            currentCommentType = CommentType.MULTI_LINE
                            multiLineCommentDepth++
                        } else if (currentCommentType == CommentType.MULTI_LINE) {
                            this.pos++
                            continue
                        } else {
                            return null
                        }
                        commentStart = this.pos
                        this.pos += 2
                        continue
                    }
                    // are we in a multi line comment of any depth?
                    if (multiLineCommentDepth > 0) {
                        this.pos++
                        continue
                    }
                    return null
                }

                else -> {
                    if (currentCommentType == CommentType.MULTI_LINE) {
                        if (peek() == '*' && hasMore(1) && peek(1) == '/') {
                            this.pos += 2
                            multiLineCommentDepth--
                            currentCommentType = if (multiLineCommentDepth == 0) null else CommentType.MULTI_LINE
                        } else {
                            this.pos++
                        }
                        continue
                    } else if (currentCommentType == CommentType.SINGLE_LINE) {
                        this.pos++
                        continue
                    }
                    return null
                }
            }
        }
        if (!hasMore(0) && currentCommentType == CommentType.MULTI_LINE) {
            return ErrorToken(this.source.substring(commentStart), buildSpan(0))
        }
        return null
    }

    private fun separator(parenOpen: SeparatorType): Separator {
        return Separator(parenOpen, buildSpan(1))
    }

    private fun lexIdentifierOrKeyword(): Token {
        var off = 1
        while (hasMore(off) && isIdentifierChar(peek(off))) {
            off++
        }
        val id = this.source.substring(this.pos, this.pos + off)
        // This is a naive solution. Using a better data structure (hashmap, trie) likely performs better.
        for (value in KeywordType.entries) {
            if (value.keyword == id) {
                return Keyword(value, buildSpan(off))
            }
        }
        return Identifier(id, buildSpan(off))
    }

    private fun lexNumber(): Token {
        if (this.isHexPrefix) {
            var off = 2
            while (hasMore(off) && isHex(peek(off))) {
                off++
            }
            if (off == 2) {
                // 0x without any further hex digits
                return ErrorToken(this.source.substring(this.pos, this.pos + off), buildSpan(2))
            }
            return NumberLiteral(this.source.substring(this.pos, this.pos + off), 16, buildSpan(off))
        }
        var off = 1
        while (hasMore(off) && isNumeric(peek(off))) {
            off++
        }
        if (peek() == '0' && off > 1) {
            // leading zero is not allowed
            return ErrorToken(this.source.substring(this.pos, this.pos + off), buildSpan(off))
        }
        return NumberLiteral(this.source.substring(this.pos, this.pos + off), 10, buildSpan(off))
    }

    private val isHexPrefix: Boolean
        get() = peek() == '0' && hasMore(1) && (peek(1) == 'x' || peek(1) == 'X')

    private fun isIdentifierChar(c: Char): Boolean {
        return c == '_' || c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9'
    }

    private fun isNumeric(c: Char): Boolean {
        return c >= '0' && c <= '9'
    }

    private fun isHex(c: Char): Boolean {
        return isNumeric(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
    }

    private fun singleOrWithEquals(single: Operator.OperatorType, withEquals: Operator.OperatorType): Token {
        if (hasMore(1) && peek(1) == '=') {
            return Operator(withEquals, buildSpan(2))
        }
        return Operator(single, buildSpan(1))
    }

    private fun singleOrDoubleOrWithEquals(
        char: Char,
        single: Operator.OperatorType,
        double: Operator.OperatorType,
        withEquals: Operator.OperatorType
    ): Token {
        if (hasMore(1) && peek(1) == char) {
            return Operator(double, buildSpan(2))
        } else if (hasMore(1) && peek(1) == '=') {
            return Operator(withEquals, buildSpan(2))
        }
        return Operator(single, buildSpan(1))
    }

    private fun buildSpan(proceed: Int): Span {
        val start = this.pos
        this.pos += proceed
        val s = SimplePosition(this.line, start - this.lineStart)
        val e = SimplePosition(this.line, start - this.lineStart + proceed)
        return SimpleSpan(s, e)
    }

    private fun peek(): Char {
        return this.source.get(this.pos)
    }

    private fun hasMore(offset: Int): Boolean {
        return this.pos + offset < this.source.length
    }

    private fun peek(offset: Int): Char {
        return this.source[this.pos + offset]
    }

    companion object {
        fun forString(source: String): Lexer {
            return Lexer(source)
        }
    }
}
