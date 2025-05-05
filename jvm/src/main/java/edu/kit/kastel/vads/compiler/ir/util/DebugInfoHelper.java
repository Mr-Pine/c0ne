package edu.kit.kastel.vads.compiler.ir.util;

/// This is a dirty trick as we don't have Scoped Values.
/// It allows tracking debug info without having to pass it
/// down all the layers.
public final class DebugInfoHelper {
    private static DebugInfo debugInfo = DebugInfo.NoInfo.INSTANCE;

    public static void setDebugInfo(DebugInfo debugInfo) {
        DebugInfoHelper.debugInfo = debugInfo;
    }

    public static DebugInfo getDebugInfo() {
        return debugInfo;
    }
}
