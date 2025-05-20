package edu.kit.kastel.vads.compiler.ir.util;

import de.mr_pine.c0ne.Span;

/// Provides information to ease debugging
public sealed interface DebugInfo {
    enum NoInfo implements DebugInfo {
        INSTANCE
    }

    record SourceInfo(Span span) implements DebugInfo {}
}
