package de.mr_pine.c0ne.parser.type

interface BasicType: Type {
    object Integer: Type {
        override fun asString() = "int"
    }
    object Boolean: Type {
        override fun asString() = "bool"
    }
}