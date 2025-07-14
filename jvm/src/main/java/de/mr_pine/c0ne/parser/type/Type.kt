package de.mr_pine.c0ne.parser.type

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
    }
}