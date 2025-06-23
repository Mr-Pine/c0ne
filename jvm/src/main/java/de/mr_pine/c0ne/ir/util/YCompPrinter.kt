package de.mr_pine.c0ne.ir.util

import de.mr_pine.c0ne.analysis.nodesInControlFlowOrder
import de.mr_pine.c0ne.backend.RegisterAllocator
import de.mr_pine.c0ne.backend.Schedule
import de.mr_pine.c0ne.backend.x86.instructions.Call
import de.mr_pine.c0ne.ir.IrGraph
import de.mr_pine.c0ne.ir.node.*
import de.mr_pine.c0ne.ir.node.ProjNode.SimpleProjectionInfo
import java.util.*

class YCompPrinter(
    private val graph: IrGraph,
    private val schedule: Schedule?,
    val registers: RegisterAllocator.RegisterAllocation<*>?
) {
    private val clusters: MutableMap<Block, MutableSet<Node>> = mutableMapOf()
    private val ids: MutableMap<Node, Int> = mutableMapOf()
    private var nodeCounter = 0
    private var blockCounter = 0

    private fun String.indent(n: Int): String {
        var result = this
        repeat(n) { result = this.prependIndent("  ") }
        return result
    }

    private fun prepare(node: Node, seen: MutableSet<Node>) {
        if (!seen.add(node)) {
            return
        }

        if (node !is Block) {
            this.clusters.computeIfAbsent(
                node.block
            ) {
                Collections.newSetFromMap(
                    IdentityHashMap()
                )
            }.add(node)
            prepare(node.block, seen)
        }
        for (predecessor in node.predecessors()) {
            prepare(predecessor, seen)
        }
        if (node === this.graph.endBlock) {
            this.clusters.put(this.graph.endBlock, mutableSetOf())
        }
    }

    private fun dumpGraphAsString() = buildString {

        appendLine(
            """
            graph: {
              title: "${graph.name()}"
              display_edge_labels: yes
              layoutalgorithm: mindepth //${'$'} "Compilergraph"
              manhattan_edges: yes
              port_sharing: no
              orientation: top_to_bottom
                
        """.trimIndent()
        )

        for (color in VcgColor.entries) {
            appendLine("colorentry ${color.id()}: ${color.rgb}".indent(1))
        }

        appendLine().append(formatMethod(graph.name()).indent(1))

        appendLine().append(formatBlockSchedule().indent(1))

        append("}")
    }

    private fun formatMethod(name: String) = buildString {

        appendLine(
            """
            graph: {
              title: "method"
              label: "$name"
              color: ${VcgColor.ROOT_BLOCK.id()}
            """.trimIndent()
        )

        for (entry in this@YCompPrinter.clusters.entries) {
            appendLine().append(formatBlock(entry.key, entry.value).indent(1))
        }

        append("}")
    }

    private fun formatBlock(block: Block, nodes: MutableSet<Node>) = buildString {
        appendLine(
            """
            graph: {
              title: "${nodeTitle(block)}"
              label: "${nodeLabel(block)}"
              color: ${VcgColor.BLOCK.id()}
            """.trimIndent()
        )

        for (node in nodes) {
            appendLine(formatNode(node).indent(1))
            appendLine(formatInputEdges(node).indent(1))
        }
        append(formatControlflowEdges(block))

        append(formatSchedule(block))

        appendLine().append("}")
    }

    private fun formatNode(node: Node): String {
        val infoText = "I am an info text for $node"

        return """
            node: {
              title: "${nodeTitle(node)}"
              label: "${nodeLabel(node)}"
              color: ${nodeColor(node).id()}
              info1: "$infoText"
            }
        """.trimIndent()
    }

    private fun formatInputEdges(node: Node): String {
        val edges = (0..<node.predecessors().size).map { index -> Edge(node.predecessor(index), node, index) }
        return formatEdges(edges, listOf("priority: 50"))
    }

    private fun formatControlflowEdges(block: Block): String? {
        val result = StringJoiner("\n")
        val parents = block.predecessors()
        for ((i, parent) in parents.withIndex()) {
            if (parent is ReturnNode) {
                // Return needs no label
                result.add(formatControlflowEdge(parent, block, "$i"))
            } else if (parent is ProjNode && parent.projectionInfo() in listOf(
                    SimpleProjectionInfo.IF_TRUE,
                    SimpleProjectionInfo.IF_FALSE
                )
                || parent is JumpNode || parent is IfNode
            ) {
                result.add(formatControlflowEdge(parent, block, "$i"))
            } else {
                throw RuntimeException("Unknown parent type: $parent")
            }
        }

        return result.toString()
    }

    private fun formatControlflowEdge(source: Node, dst: Block, label: String) = """
            edge: {
              sourcename: "${nodeTitle(source)}"
              targetname: "${nodeTitle(dst)}"
              label: "$label"
              color: ${VcgColor.CONTROL_FLOW.id()}
            }
        """.trimIndent()


    private val Node.isSideeffect: Boolean
        get() = this is ProjNode && this.projectionInfo() === SimpleProjectionInfo.SIDE_EFFECT || this is Phi && this.isSideEffectPhi

    private fun formatEdges(edges: Collection<Edge>, additionalProps: List<String>) = edges.joinToString("\n") { edge ->
        // edge: {sourcename: "n74" targetname: "n71" label: "0" class:14 priority:50 color:blue}
        val isSideeffect = edge.src.isSideeffect || edge.dst.isSideeffect

        val extraProps = additionalProps.toMutableList()
        if (isSideeffect) {
            extraProps.addFirst("color: ${VcgColor.MEMORY.id()}")
        }
        if (edge.src is IfNode) {
            extraProps.addFirst("color: ${VcgColor.CONTROL_FLOW.id()}")
        }
        """
            edge: {
              sourcename: "${nodeTitle(edge.src)}"
              targetname: "${nodeTitle(edge.dst)}"
              label: "${edge.index}"
            """.trimIndent() + extraProps.joinToString("\n") {
            it.indent(1)
        } + "}"
    }

    private fun formatBlockSchedule(): String {
        if (schedule == null) return ""
        val edges = schedule.blockOrder.windowed(2).mapIndexed { i, (src, dst) -> Edge(src, dst, i) }
        return formatEdges(edges, listOf("color: ${VcgColor.SCHEDULE.id()}"))
    }

    private fun formatSchedule(block: Block): String {
        // Once you have a schedule, you might want to also emit it :)
        if (schedule == null) return ""
        val blockSchedule = schedule.blockSchedules[block] ?: return ""
        val edges = blockSchedule.nodeOrder.windowed(2).mapIndexed { i, (src, dst) ->
            Edge(src, dst, i)
        }
        return formatEdges(edges, listOf("color: ${VcgColor.SCHEDULE.id()}"))
    }

    private fun nodeColor(node: Node): VcgColor {
        return when (node) {
            is BinaryOperationNode, is UnaryOperationNode, is Block, is ConstIntNode, is ConstBoolNode -> VcgColor.NORMAL
            is Phi -> VcgColor.PHI
            is ProjNode -> {
                if (node.projectionInfo() == SimpleProjectionInfo.SIDE_EFFECT) {
                    VcgColor.MEMORY
                } else if (node.projectionInfo() == SimpleProjectionInfo.RESULT) {
                    VcgColor.NORMAL
                } else if (node.projectionInfo() == SimpleProjectionInfo.IF_TRUE || node.projectionInfo() == SimpleProjectionInfo.IF_FALSE) {
                    VcgColor.CONTROL_FLOW
                } else {
                    VcgColor.NORMAL
                }
            }

            is UndefNode -> VcgColor.SPECIAL

            is CallNode -> VcgColor.CONTROL_FLOW
            is ReturnNode -> VcgColor.CONTROL_FLOW
            is StartNode -> VcgColor.CONTROL_FLOW
            is IfNode -> VcgColor.CONTROL_FLOW
            is JumpNode -> VcgColor.CONTROL_FLOW
        }
    }

    private fun nodeTitle(node: Node): String {
        if (node is Block) {
            if (node == this.graph.startBlock) {
                return "start-block"
            } else if (node == this.graph.endBlock) {
                return "end-block"
            }
            return "block-" + idFor(node)
        }
        return "node-" + idFor(node)
    }

    private fun nodeLabel(node: Node): String {
        if (node is Block) {
            return "block ${node.label}"
        } else if (registers != null && registers.getOrNull(node) != null) {
            return "${node}\n${registers[node]}"
        }
        return node.toString()
    }

    private fun idFor(node: Node): Int {
        if (node is Block) {
            return this.ids.computeIfAbsent(
                node
            ) { this.blockCounter++ }
        }
        return this.ids.computeIfAbsent(
            node
        ) { this.nodeCounter++ }
    }

    private data class Edge(val src: Node, val dst: Node, val index: Int)

    private enum class VcgColor(val rgb: String) {
        // colorentry 100: 204 204 204  gray
        // colorentry 101: 222 239 234  faint green
        // colorentry 103: 242 242 242  white-ish
        // colorentry 104: 153 255 153  light green
        // colorentry 105: 153 153 255  blue
        // colorentry 106: 255 153 153  red
        // colorentry 107: 255 255 153  yellow
        // colorentry 108: 255 153 255  pink
        // colorentry 110: 127 127 127  dark gray
        // colorentry 111: 153 255 153  light green
        // colorentry 114: 153 153 255  blue
        CONTROL_FLOW("255 153 153"), MEMORY("153 153 255"), NORMAL("242 242 242"), SPECIAL("255 153 255"), CONST("255 255 153"), PHI(
            "153 255 153"
        ),
        ROOT_BLOCK("204 204 204"), BLOCK("222 239 234"), SCHEDULE("255 153 255");

        fun id(): Int {
            return 100 + ordinal
        }
    }

    companion object {
        fun print(graph: IrGraph, schedule: Schedule? = null, registers: RegisterAllocator.RegisterAllocation<*>? = null): String {
            val printer = YCompPrinter(graph, schedule, registers)
            printer.prepare(graph.endBlock, HashSet<Node>())
            return printer.dumpGraphAsString()
        }
    }
}
