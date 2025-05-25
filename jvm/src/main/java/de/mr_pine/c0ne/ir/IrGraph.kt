package de.mr_pine.c0ne.ir

import de.mr_pine.c0ne.ir.node.Block
import de.mr_pine.c0ne.ir.node.Node
import java.util.IdentityHashMap
import java.util.SequencedSet

class IrGraph(private val name: String) {
    private val successors: MutableMap<Node, SequencedSet<Node>> = IdentityHashMap()
    val startBlock: Block = Block(this, "start")
    val endBlock: Block = Block(this, "end")

    fun registerSuccessor(node: Node, successor: Node) {
        this.successors.computeIfAbsent(node) { LinkedHashSet() }
            .add(successor)
    }

    fun removeSuccessor(node: Node, oldSuccessor: Node) {
        this.successors.computeIfAbsent(node) { LinkedHashSet() }
            .remove(oldSuccessor)
    }

    /** {@return the set of nodes that have the given node as one of their inputs} */
    fun successors(node: Node): Set<Node> {
        val successors = this.successors[node]
        if (successors == null) {
            return setOf()
        }
        return successors.toSet()
    }

    /** {@return the name of this graph} */
    fun name(): String {
        return name
    }
}
