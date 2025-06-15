package de.mr_pine.c0ne.backend

import de.mr_pine.c0ne.ir.node.Block
import de.mr_pine.c0ne.ir.node.Node

class AllocationInterferenceGraph(schedule: Schedule, startBlock: Block) {
    private val interferenceGraph = mutableMapOf<Node, MutableSet<Node>>()

    init {
        val livenessAnalysis = LivenessAnalysis(startBlock, schedule)
        livenessAnalysis.analyze()
        val livenessMap = livenessAnalysis.result

        for ((nodeInBlock, live) in livenessMap.filter { (key, _) -> key.node.needsRegister }) {
            val node = nodeInBlock.node
            val nodeInterferences = interferenceGraph.getOrPut(node) { mutableSetOf() }
            val liveNodes = live.inValue.filter(Node::needsRegister)
            for (liveNode in liveNodes) {
                if (liveNode != node) nodeInterferences.add(liveNode)
                interferenceGraph.getOrPut(liveNode) { mutableSetOf() }.apply {
                    if (liveNode != node) add(node)
                    addAll(liveNodes - liveNode)
                }
            }
        }
    }

    fun neighbourhood(node: Node) = interferenceGraph[node] ?: emptySet()

    fun buildSimplicialOrdering() = buildList {
        val nodes = interferenceGraph.keys.filter(Node::needsRegister).associateWith { 0 }.toMutableMap()
        while (nodes.isNotEmpty()) {
            val maxNode = nodes.maxBy { it.value }.key
            nodes.remove(maxNode)
            add(maxNode)
            val neighbourhood = neighbourhood(maxNode) intersect nodes.keys
            for (neighbour in neighbourhood) {
                nodes[neighbour] = nodes[neighbour]!! + 1
            }
        }
    }
}