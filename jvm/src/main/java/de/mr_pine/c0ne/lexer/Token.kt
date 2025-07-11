package de.mr_pine.c0ne.lexer

import de.mr_pine.c0ne.lexer.Separator.SeparatorType
import de.mr_pine.c0ne.Span

sealed interface Token {
    val span: Span

    fun isKeyword(keywordType: KeywordType): Boolean {
        return false
    }

    fun isOperator(operatorType: Operator.OperatorType): Boolean {
        return false
    }

    fun isSeparator(separatorType: SeparatorType): Boolean {
        return false
    }

    fun asString(): String
}
