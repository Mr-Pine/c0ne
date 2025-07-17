package de.mr_pine.c0ne.parser.type

import de.mr_pine.c0ne.lexer.Identifier

data class PointerType(val baseType: Type): Type.SmallType {
    override fun asString() = "${baseType.asString()}*"

    override val smallSize = 8

    override fun compatibleWith(other: Type): Boolean {
        return other is PointerType && baseType.compatibleWith(other.baseType)
    }
}