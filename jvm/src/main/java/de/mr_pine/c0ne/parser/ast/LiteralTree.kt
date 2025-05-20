package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.lexer.Keyword
import edu.kit.kastel.vads.compiler.lexer.KeywordType
import edu.kit.kastel.vads.compiler.parser.visitor.Visitor
import java.lang.Long.parseLong

sealed interface LiteralTree<T> : ExpressionTree {
    fun parseValue(): T?

    @JvmRecord
    data class LiteralIntTree(@JvmField val value: String, val base: Int, override val span: Span) : LiteralTree<Long> {

        override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
            return visitor.visit(this, data)
        }

        override fun parseValue(): Long? {
            var begin = 0
            val end = value.length
            if (base == 16) {
                begin = 2 // ignore 0x
            }
            val l = try {
                parseLong(value.substring(begin, end), base)
            } catch (_: NumberFormatException) {
                return null
            }
            val isNegative = l < 0
            val validPositiveInt = l <= Integer.toUnsignedLong(Int.Companion.MIN_VALUE)
            val validInt = l <= Integer.toUnsignedLong(-1)
            if (isNegative || (!validPositiveInt && base != 16) || !validInt) {
                return null
            }
            return l
        }
    }

    data class LiteralBoolTree(val value: Keyword) : LiteralTree<Boolean> {
        override val span: Span
            get() = value.span

        override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
            return visitor.visit(this, data)
        }

        override fun parseValue(): Boolean {
            return when (value.type) {
                KeywordType.TRUE -> true
                KeywordType.FALSE -> false
                else -> throw IllegalArgumentException("Invalid keyword type ${value.type} for boolean literal")
            }
        }
    }
}
