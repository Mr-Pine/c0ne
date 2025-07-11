package de.mr_pine.c0ne.parser.type

sealed interface BasicType: Type {
    object Integer: Type {
        override fun asString() = "int"
        override fun toString() = asString()
    }
    object Boolean: Type {
        override fun asString() = "bool"
        override fun toString() = asString()
    }
}