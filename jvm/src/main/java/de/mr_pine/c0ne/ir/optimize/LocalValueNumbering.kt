package de.mr_pine.c0ne.ir.optimize

import de.mr_pine.c0ne.ir.node.Node

/** This depends on [Node.equals] and [Node.hashCode] methods.
 * As long as they take the block into account, it is only local, but replacement
 * is extremely simple.
 * When using classes like [HashMap] or [java.util.HashSet] without this optimization,
 * the [Node.equals] and [Node.hashCode] methods must be adjusted. */
class LocalValueNumbering : Optimizer {
    private val knownNodes: MutableMap<Node, Node> = mutableMapOf()

    override fun transform(node: Node): Node {
        return this.knownNodes.computeIfAbsent(node) { it }
    }
}
