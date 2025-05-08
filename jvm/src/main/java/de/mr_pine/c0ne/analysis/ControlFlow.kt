package de.mr_pine.c0ne.analysis

import edu.kit.kastel.vads.compiler.ir.IrGraph
import edu.kit.kastel.vads.compiler.ir.node.Node

fun IrGraph.nodesInControlFlowOrder(): List<Node> {
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

abstract class BackwardsControlFlow<Output>: BackwardsFlow<Output>() {
    val controlFlowOrders = mutableMapOf<IrGraph, List<Node>>()
    override fun analyze(graph: IrGraph) {
        controlFlowOrders[graph] = graph.nodesInControlFlowOrder()
        super.analyze(graph)
    }

    context(graph: IrGraph)
    override fun predecessors(node: Node): List<Node> {
        val nodes = controlFlowOrders[graph]!!
        val currentIndex = nodes.indexOf(node)
        return listOfNotNull(nodes.getOrNull(currentIndex - 1))
    }
}