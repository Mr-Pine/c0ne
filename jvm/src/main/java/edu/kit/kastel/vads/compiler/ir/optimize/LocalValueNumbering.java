package edu.kit.kastel.vads.compiler.ir.optimize;

import edu.kit.kastel.vads.compiler.ir.node.Node;

import java.util.HashMap;
import java.util.Map;

/// This depends on [Node#equals(java.lang.Object)] and  [Node#hashCode()] methods.
/// As long as they take the block into account, it is only local, but replacement
/// is extremely simple.
/// When using classes like [HashMap] or [java.util.HashSet] without this optimization,
/// the [Node#equals(java.lang.Object)] and  [Node#hashCode()] methods must be adjusted.
public class LocalValueNumbering implements Optimizer {
    private final Map<Node, Node> knownNodes = new HashMap<>();

    @Override
    public Node transform(Node node) {
        return this.knownNodes.computeIfAbsent(node, n -> n);
    }
}
