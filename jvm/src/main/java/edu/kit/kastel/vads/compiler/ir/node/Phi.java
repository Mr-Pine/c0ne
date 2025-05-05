package edu.kit.kastel.vads.compiler.ir.node;

public final class Phi extends Node {
    public Phi(Block block) {
        super(block);
    }

    public void appendOperand(Node node) {
        addPredecessor(node);
    }
}
