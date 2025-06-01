package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.IrGraph
import de.mr_pine.c0ne.ir.visitor.SSAVisitor

class Block(graph: IrGraph, val label: String) : Node(graph) {
    override fun toString() = "Block $label"

    fun removeSuccessor(node: Node, irGraph: IrGraph) {
        val edges = irGraph.allSuccessors.entries.flatMap { (pred, succs) -> succs.map { pred to it } }
            .filter { it.first.block == this }.filter { it.second is Phi || it.second is Block }
        for ((pred, succ) in edges) {
            succ.removePredecessor(pred)
        }
    }

    override fun accept(visitor: SSAVisitor) {
        visitor.visit(this)
    }
}
