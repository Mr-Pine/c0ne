package de.mr_pine.c0ne.parser.type

import de.mr_pine.c0ne.lexer.Identifier

data class PointerType(val baseType: Type): Type {
    override fun asString() = "${baseType.asString()}*"

    override val smallSize = 8
}