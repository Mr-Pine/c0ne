package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import edu.kit.kastel.vads.compiler.parser.visitor.Visitor
import java.lang.Long.parseLong

@JvmRecord
data class LiteralTree(@JvmField val value: String, val base: Int, override val span: Span) : ExpressionTree {

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }

    fun parseValue(): Long? {
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
