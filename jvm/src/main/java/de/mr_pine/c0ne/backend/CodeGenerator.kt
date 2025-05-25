package de.mr_pine.c0ne.backend

import de.mr_pine.c0ne.analysis.nodesInControlFlowOrder
import de.mr_pine.c0ne.ir.util.YCompPrinter
import de.mr_pine.c0ne.ir.IrGraph
import de.mr_pine.c0ne.ir.node.AddNode
import de.mr_pine.c0ne.ir.node.ArithmeticLeftShift
import de.mr_pine.c0ne.ir.node.ArithmeticRightShift
import de.mr_pine.c0ne.ir.node.BitwiseNotNode
import de.mr_pine.c0ne.ir.node.Block
import de.mr_pine.c0ne.ir.node.ConstBoolNode
import de.mr_pine.c0ne.ir.node.ConstIntNode
import de.mr_pine.c0ne.ir.node.DivNode
import de.mr_pine.c0ne.ir.node.EqualsNode
import de.mr_pine.c0ne.ir.node.IfNode
import de.mr_pine.c0ne.ir.node.JumpNode
import de.mr_pine.c0ne.ir.node.LessThanEqNode
import de.mr_pine.c0ne.ir.node.LessThanNode
import de.mr_pine.c0ne.ir.node.ModNode
import de.mr_pine.c0ne.ir.node.MulNode
import de.mr_pine.c0ne.ir.node.Node
import de.mr_pine.c0ne.ir.node.NotNode
import de.mr_pine.c0ne.ir.node.Phi
import de.mr_pine.c0ne.ir.node.ProjNode
import de.mr_pine.c0ne.ir.node.ReturnNode
import de.mr_pine.c0ne.ir.node.StartNode
import de.mr_pine.c0ne.ir.node.SubNode
import java.io.File

interface CodeGenerator<R: Register, A : RegisterAllocator.RegisterAllocation<R>> {
    fun getAllocator(): RegisterAllocator<R, A>


    fun generateCode(graphs: List<IrGraph>): ByteArray {
        val generated = buildString {
            prologue()
            for (graph in graphs) {
                val registers = getAllocator().allocateRegisters(graph)
                File("/tmp/graph.vcg").writeText(YCompPrinter.print(graph, registers))
                with(registers) {
                    functionPrologue(graph.name())
                    codegenGraph(graph)
                    functionEpilogue(graph.name())
                }
            }
        }
        return postprocess(generated)
    }

    fun postprocess(generation: String): ByteArray

    context(builder: StringBuilder)
    fun prologue()

    context(builder: StringBuilder, registers: A)
    fun functionPrologue(name: String)

    context(builder: StringBuilder, registers: A)
    fun functionEpilogue(name: String)

    context(builder: StringBuilder, registers: A)
    fun codegenGraph(irGraph: IrGraph) {
        val nodes = irGraph.nodesInControlFlowOrder()
        for (node in nodes) {
            when (node) {
                is AddNode -> processNode(node)
                is DivNode -> processNode(node)
                is ModNode -> processNode(node)
                is MulNode -> processNode(node)
                is SubNode -> processNode(node)
                is ArithmeticLeftShift -> TODO()
                is ArithmeticRightShift -> TODO()
                is EqualsNode -> TODO()
                is LessThanEqNode -> TODO()
                is LessThanNode -> TODO()
                is BitwiseNotNode -> TODO()
                is NotNode -> TODO()
                is Block -> processNode(node)
                is ConstIntNode -> processNode(node)
                is ConstBoolNode -> TODO()
                is Phi -> processNode(node)
                is ProjNode -> processNode(node)
                is ReturnNode -> processNode(node)
                is IfNode -> TODO()
                is JumpNode -> TODO()
                is StartNode -> processNode(node)
            }
        }
    }

    context(builder: StringBuilder, registers: A)
    fun processNode(node: AddNode)

    context(builder: StringBuilder, registers: A)
    fun processNode(node: SubNode)

    context(builder: StringBuilder, registers: A)
    fun processNode(node: DivNode)

    context(builder: StringBuilder, registers: A)
    fun processNode(node: ModNode)

    context(builder: StringBuilder, registers: A)
    fun processNode(node: MulNode)

    context(builder: StringBuilder, registers: A)
    fun processNode(node: ConstIntNode)

    context(builder: StringBuilder, registers: A)
    fun processNode(node: Block)

    /*context(builder: StringBuilder, registers: A)
    fun processNode(node: Phi)*/

    context(builder: StringBuilder, registers: A)
    fun processNode(node: ProjNode)

    context(builder: StringBuilder, registers: A)
    fun processNode(node: ReturnNode)

    context(builder: StringBuilder, registers: A)
    fun processNode(node: StartNode)

    context(builder: StringBuilder, registers: A)
    fun processNode(node: Node) {
        throw Exception("Can't process abstract node: $node")
    }
}