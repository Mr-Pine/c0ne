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
            get() = if (smallSize == 1) 1 else if (smallSize == 4) 4 else 8
    }
}