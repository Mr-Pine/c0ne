package edu.kit.kastel.vads.compiler.ir.node;

public final class ReturnNode extends Node {
    public static final int SIDE_EFFECT = 0;
    public static final int RESULT = 1;
    public ReturnNode(Block block, Node sideEffect, Node result) {
        super(block, sideEffect, result);
    }
}
