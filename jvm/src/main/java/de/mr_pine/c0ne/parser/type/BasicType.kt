package de.mr_pine.c0ne.parser.type

sealed interface BasicType: Type.SmallType {
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