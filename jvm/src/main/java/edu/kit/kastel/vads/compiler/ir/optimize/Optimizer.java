package edu.kit.kastel.vads.compiler.ir.optimize;

import edu.kit.kastel.vads.compiler.ir.node.Node;

/// An interface that allows replacing a node with a more optimal one.
public interface Optimizer {

    Node transform(Node node);
}
