package de.mr_pine.c0ne.analysis

import de.mr_pine.c0ne.backend.Schedule
import de.mr_pine.c0ne.ir.IrGraph
import de.mr_pine.c0ne.ir.node.Block
import de.mr_pine.c0ne.ir.node.Node
import de.mr_pine.c0ne.ir.node.ReturnNode

abstract class BackwardsFlow<InValue, OutValue>(protected val schedule: Schedule) {
    private val computedInputs = mutableMapOf<NodeInBlock, List<InValue?>>()
    private val outValues = mutableMapOf<NodeInBlock, OutValue>()
    private val inValues = mutableMapOf<NodeInBlock, InValue>()

    data class NodeInBlock(val node: Node, val block: Block)

    data class BackwardsFlowResult<InValue, OutValue>(val inValue: InValue, val outValue: OutValue)

    private val queue = mutableListOf<NodeInBlock>()

    val result: Map<NodeInBlock, BackwardsFlowResult<InValue, OutValue>>
        get() = buildMap {
            for (key in outValues.keys) {
                put(key, BackwardsFlowResult(inValues[key]!!, outValues[key]!!))
            }
        }

    open fun analyze() {
        for ((block, node) in schedule.blockSchedules.mapNotNull { (key, value) ->
            value.nodeOrder.firstNotNullOfOrNull { it as? ReturnNode }?.let { key to it }
        }) {
            queue.add(NodeInBlock(node, block))
        }
        while (queue.isNotEmpty()) {
            analyzeNode(queue.removeFirst())
        }
    }

    private fun analyzeNode(nodeInBlock: NodeInBlock) {
        val inputs = successors(nodeInBlock).map { inValues[it] }
        if (computedInputs[nodeInBlock] == inputs) return
        val outValue = computeOutValue(nodeInBlock, inputs)
        outValues[nodeInBlock] = outValue
        inValues[nodeInBlock] = computeInValue(nodeInBlock, outValue)
        computedInputs[nodeInBlock] = inputs
        for (predecessor in predecessors(nodeInBlock)) {
            queue.add(predecessor)
        }
    }

    abstract fun predecessors(nodeInBlock: NodeInBlock): List<NodeInBlock>
    abstract fun successors(nodeInBlock: NodeInBlock): List<NodeInBlock>
    abstract fun computeOutValue(nodeInBlock: NodeInBlock, inputs: List<InValue?>): OutValue
    abstract fun computeInValue(nodeInBlock: NodeInBlock, outValue: OutValue): InValue
}