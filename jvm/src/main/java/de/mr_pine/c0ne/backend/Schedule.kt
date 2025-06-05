package de.mr_pine.c0ne.backend

import de.mr_pine.c0ne.ir.IrGraph
import de.mr_pine.c0ne.ir.node.*

class Schedule(private val irGraph: IrGraph) {
    val successorMap = irGraph.allSuccessors

    inner class Dfs {
        var nextFinishNumber = 0
        val visited = mutableSetOf<Block>()
        val finishNumbers = mutableMapOf<Block, Int>()

        fun dfs(block: Block) {
            if (block in visited) return
            visited.add(block)

            for (successor in block.successors) {
                dfs(successor)
            }

            finishNumbers[block] = nextFinishNumber
            nextFinishNumber++
        }
    }

    val blockOrder = Dfs().apply { dfs(irGraph.startBlock) }.finishNumbers.entries.sortedBy { -it.value }.map { it.key }
    val blockSchedules = calculateBlockSchedules(successorMap.keys.map { it.block }.toSet())

    fun calculateBlockSchedules(blocks: Collection<Block>): Map<Block, BlockSchedule> {
        val relevantNodes = mutableMapOf<Block, MutableList<Node>>()

        val visited = mutableSetOf<Node>()
        val todo: MutableList<Node> = (blocks.mapNotNull { it.exitNode }).toMutableList()
        while (todo.isNotEmpty()) {
            val node = todo.removeFirst()
            if (node in visited || node is Block) continue
            visited.add(node)
            if (node is Phi) {
                val phiPredecessors = node.predecessors()
                val blockPredecessors = node.block.predecessors()
                assert(phiPredecessors.size == blockPredecessors.size) { "Phi predecessors and block predecessors don't match" }
                if (!node.isSideEffectPhi) {
                    for (predBlock in blockPredecessors.map { it.block }.toSet()) {
                        relevantNodes[predBlock]!!.add(node)
                    }
                }
            } else {
                relevantNodes.getOrPut(node.block) { mutableListOf() }.add(node)
            }
            todo.addAll(node.predecessors())
        }

        return buildMap {
            for ((block, nodes) in relevantNodes) {
                put(
                    block,
                    BlockSchedule(block, nodes)
                )
            }
        }
    }

    class BlockSchedule(private val block: Block, private val nodes: MutableList<Node>) {
        val nodeOrder = sortNodes(Dfs().apply {
            while (nodes.isNotEmpty()) {
                dfs(nodes.first())
            }
        })

        private fun sortNodes(dfs: Dfs): List<Node> {
            val finishNumbers = dfs.finishNumbers
            val maxFinishNumber = dfs.nextFinishNumber - 1
            val mappedFinishNumbers = finishNumbers.mapValues {
                when (it.key) {
                    is ExitNode -> Int.MAX_VALUE
                    is StartNode -> Int.MIN_VALUE
                    is Phi -> it.value + maxFinishNumber
                    else -> it.value
                }
            }
            return mappedFinishNumbers.entries.sortedBy { it.value }
                .map { it.key }
        }

        inner class Dfs {
            var nextFinishNumber = 0
            val visited = mutableSetOf<Node>()
            val finishNumbers = mutableMapOf<Node, Int>()

            fun dfs(node: Node) {
                if (node in visited) return
                visited.add(node)
                nodes.remove(node)
                for (predecessor in node.predecessors().filter { it.block == block && it !is Phi }) {
                    dfs(predecessor)
                }

                finishNumbers[node] = nextFinishNumber
                nextFinishNumber++
            }
        }
    }

    private val Block.exitNode: ExitNode?
        get() = successorMap.keys.filter { it.block == this }.firstNotNullOfOrNull { it as? ExitNode }

    private val Block.successors: Set<Block>
        get() {
            if (this == irGraph.endBlock) return emptySet()
            val exitNode: ExitNode = exitNode!!
            val successors =
                irGraph.successors(exitNode).map { if (it is ProjNode) irGraph.successors(it).first() else it }
            return successors.map { it.block }.toSet()
        }
}