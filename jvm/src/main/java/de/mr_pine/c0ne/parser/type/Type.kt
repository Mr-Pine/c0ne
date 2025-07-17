package de.mr_pine.c0ne.parser.type

import de.mr_pine.c0ne.parser.type.Type.SmallType.*

sealed interface Type {
    fun asString(): String
    val size: Int
    val alignment: Int

    sealed interface SmallType : Type {
        val smallSize: Int
        override val size
            get() = smallSize
        override val alignment: Int
            get() = 8


        object Bottom : SmallType {
            override fun asString() = "bottom"
            override val smallSize: Int
                get() = error("No size for Bottom")
            override val alignment: Int
                get() = error("No alignment for Bottom")

            override fun compatibleWith(other: Type): Boolean {
                return other != TypeError
            }
        }

        object TypeError : SmallType {
            override fun asString() = "error"
            override val smallSize: Int
                get() = error("No size for error")
            override val alignment: Int
                get() = error("No alignment for error")

            override fun compatibleWith(other: Type): Boolean {
                return false
            }
        }
    }

    fun compatibleWith(other: Type): Boolean

    infix fun incompatibleWith(other: Type) = !compatibleWith(other)

    companion object {
        fun commonType(first: Type, second: Type): Type {
            if (first == second) return first
            if (first == Bottom || second == Bottom) return Bottom
            if (first is PointerType && second is PointerType) {
                return commonType(first.baseType, second.baseType).takeIf { it != TypeError }?.let { PointerType(it) } ?: TypeError
            }
            if (first is ArrayType && second is ArrayType) {
                return commonType(first.baseType, second.baseType).takeIf { it != TypeError }?.let { ArrayType(it) } ?: TypeError
            }
            return TypeError
        }
    }

}