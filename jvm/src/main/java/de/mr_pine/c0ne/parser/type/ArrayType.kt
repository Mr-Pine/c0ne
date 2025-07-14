package de.mr_pine.c0ne.parser.type

data class ArrayType(val baseType: Type): Type.SmallType {
    override fun asString() = "${baseType.asString()}[]"

    override val smallSize = 8

    override fun compatibleWith(other: Type): Boolean {
        return other is ArrayType && baseType.compatibleWith(other.baseType)
    }
}