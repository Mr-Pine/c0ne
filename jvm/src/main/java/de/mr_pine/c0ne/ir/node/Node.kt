package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.IrGraph
import de.mr_pine.c0ne.ir.util.DebugInfo
import de.mr_pine.c0ne.ir.util.DebugInfoHelper

/** The base class for all nodes. */
sealed class Node {
    val graph: IrGraph
    val block: Block
    private val predecessors: MutableList<Node> = mutableListOf()
    private val debugInfo: DebugInfo

    protected constructor(block: Block, vararg predecessors: Node) {
        this.graph = block.graph
        this.block = block
        this.predecessors.addAll(predecessors.toList())
        for (predecessor in predecessors) {
            graph.registerSuccessor(predecessor, this)
        }
        this.debugInfo = DebugInfoHelper.debugInfo
    }

    protected constructor(graph: IrGraph) {
        assert(this.javaClass == Block::class.java) { "must be used by Block only" }
        this.graph = graph
        this.block = this as Block
        this.debugInfo = DebugInfo.NoInfo.INSTANCE
    }

    fun predecessors(): List<Node> {
        return this.predecessors.toList()
    }

    fun setPredecessor(idx: Int, node: Node) {
        this.graph.removeSuccessor(this.predecessors[idx], this)
        this.predecessors[idx] = node
        this.graph.registerSuccessor(node, this)
    }

    fun addPredecessor(node: Node) {
        this.predecessors.add(node)
        this.graph.registerSuccessor(node, this)
    }

    fun removePredecessor(node: Node) {
        this.graph.removeSuccessor(node, this)
        this.predecessors.remove(node)
    }

    fun clearPredecessors() {
        for (predecessor in this.predecessors) {
            this.graph.removeSuccessor(predecessor, this)
        }
        this.predecessors.clear()
    }

    fun predecessor(idx: Int): Node {
        return this.predecessors[idx]
    }

    override fun toString(): String {
        return (this.javaClass.getSimpleName().replace("Node", "") + " " + info()).trimEnd()
    }

    protected open fun info(): String? {
        return ""
    }

    companion object {
        @JvmStatic
        protected fun predecessorHash(node: Node, predecessor: Int): Int {
            return System.identityHashCode(node.predecessor(predecessor))
        }
    }
}
