package de.mr_pine.c0ne.analysis

import edu.kit.kastel.vads.compiler.ir.IrGraph
import edu.kit.kastel.vads.compiler.ir.node.Node

abstract class BackwardsFlow<InValue, OutValue> {
    private val computedInputs = mutableMapOf<Node, List<InValue?>>()
    private val outValues = mutableMapOf<Node, OutValue>()
    private val inValues = mutableMapOf<Node, InValue>()

    data class BackwardsFlowResult<InValue, OutValue>(val inValue: InValue, val outValue: OutValue)
    val result: Map<Node, BackwardsFlowResult<InValue, OutValue>>
        get() = buildMap {
            for (key in outValues.keys) {
                put(key, BackwardsFlowResult(inValues[key]!!, outValues[key]!!))
            }
        }

    open fun analyze(graph: IrGraph) {
        with(graph) {
            analyzeNode(graph.endBlock())
        }
    }

    context(graph: IrGraph)
    private fun analyzeNode(node: Node) {
        val inputs = successors(node).map { inValues[it] }
        if (computedInputs[node] == inputs) return
        val outValue = computeOutValue(node, inputs)
        outValues[node] = outValue
        inValues[node] = computeInValue(node, outValue)
        computedInputs[node] = inputs
        for (predecessor in predecessors(node)) {
            analyzeNode(predecessor)
        }
    }

    abstract fun predecessors(node: Node): List<Node>
    abstract fun successors(node: Node): List<Node>
    abstract fun computeOutValue(node: Node, inputs: List<InValue?>): OutValue
    abstract fun computeInValue(node: Node, outValue: OutValue): InValue
}