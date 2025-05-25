package de.mr_pine.c0ne.ir.util

/** This is a dirty trick as we don't have Scoped Values.
 * It allows tracking debug info without having to pass it
 * down all the layers. */
object DebugInfoHelper {
    var debugInfo: DebugInfo = DebugInfo.NoInfo.INSTANCE
}
