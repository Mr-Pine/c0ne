package de.mr_pine.c0ne.analysis

import edu.kit.kastel.vads.compiler.ir.IrGraph
import edu.kit.kastel.vads.compiler.ir.node.Node

abstract class BackwardsFlow<Output> {
    private val computedInputs = mutableMapOf<Node, List<Output?>>()
    private val outputs = mutableMapOf<Node, Output>()
    val result: Map<Node, Output>
        get() = outputs

    open fun analyze(graph: IrGraph) {
        with(graph) {
            analyzeNode(graph.endBlock())
        }
    }

    context(graph: IrGraph)
    private fun analyzeNode(node: Node) {
        val inputs = predecessors(node).map { outputs[it] }
        if (computedInputs[node] == inputs) return
        val output = computeOutput(node, inputs)
        outputs[node] = output
        computedInputs[node] = inputs
        for (predecessor in predecessors(node)) {
            analyzeNode(predecessor)
        }
    }

    context(graph: IrGraph)
    abstract fun predecessors(node: Node): List<Node>
    abstract fun computeOutput(node: Node, inputs: List<Output?>): Output
}