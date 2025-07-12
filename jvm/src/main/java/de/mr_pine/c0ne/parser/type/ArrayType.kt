package de.mr_pine.c0ne.parser.type

data class ArrayType(val baseType: Type): Type.SmallType {
    override fun asString() = "${baseType.asString()}[]"

    override val smallSize = 8
}