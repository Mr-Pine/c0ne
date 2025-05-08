package de.mr_pine.c0ne.backend

import edu.kit.kastel.vads.compiler.ir.IrGraph
import edu.kit.kastel.vads.compiler.ir.node.Node

fun IrGraph.nodesInControlFlowOrder(): MutableList<Node> {
    val controlFlowOrder = mutableListOf<Node>()
    scan(endBlock(), mutableSetOf(endBlock()), controlFlowOrder)
    return controlFlowOrder
}

private fun scan(node: Node, visited: MutableSet<Node>, controlFlowOrder: MutableList<Node>) {
    for (predecessor in node.predecessors()) {
        if (predecessor !in visited) {
            visited.add(predecessor)
            scan(predecessor, visited, controlFlowOrder)
        }
    }
    controlFlowOrder.add(node)
}