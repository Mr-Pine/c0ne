package de.mr_pine.c0ne.ir.util

import de.mr_pine.c0ne.Span

/** Provides information to ease debugging */
interface DebugInfo {
    enum class NoInfo : DebugInfo {
        INSTANCE
    }

    data class SourceInfo(val span: Span) : DebugInfo
}
