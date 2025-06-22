package de.mr_pine.c0ne

interface Position {
    val line: Int
    val column: Int

    data class SimplePosition(override val line: Int, override val column: Int) : Position {
        override fun toString(): String {
            return "$line:$column"
        }
    }

    data object SyntheticPosition : Position {
        override val line = 0
        override val column = 0
    }
}
