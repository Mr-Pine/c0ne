package de.mr_pine.c0ne

interface Span {
    val start: Position
    val end: Position

    infix fun merge(later: Span): Span

    data class SimpleSpan(override val start: Position, override val end: Position) : Span {
        override fun merge(later: Span): Span {
            return SimpleSpan(this.start, later.end)
        }

        override fun toString(): String {
            return "[" + this.start + "|" + this.end + "]"
        }
    }

    object Synthetic: Span {
        override val start = Position.SyntheticPosition
        override val end = Position.SyntheticPosition

        override fun merge(later: Span) = later
    }
}
