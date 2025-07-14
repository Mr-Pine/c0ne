package de.mr_pine.c0ne.parser.type

sealed interface BasicType: Type.SmallType {

    override fun compatibleWith(other: Type): kotlin.Boolean {
        return this == other
    }

    object Integer: BasicType {
        override fun asString() = "int"
        override fun toString() = asString()

        override val smallSize = 4
    }
    object Boolean: BasicType {
        override fun asString() = "bool"
        override fun toString() = asString()

        override val smallSize = 1
    }
}