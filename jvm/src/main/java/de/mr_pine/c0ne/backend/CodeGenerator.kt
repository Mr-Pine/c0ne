package de.mr_pine.c0ne.backend

import edu.kit.kastel.vads.compiler.ir.IrGraph
import edu.kit.kastel.vads.compiler.ir.node.*

interface CodeGenerator<R: Register, A : RegisterAllocator.RegisterAllocation<R>> {
    fun getAllocator(): RegisterAllocator<R, A>


    fun generateCode(graphs: List<IrGraph>): ByteArray {
        val generated = buildString {
            prologue()
            for (graph in graphs) {
                val registers = getAllocator().allocateRegisters(graph)
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
        scan(irGraph.endBlock(), visited = mutableSetOf())
    }

    context(builder: StringBuilder, registers: A)
    fun scan(node: Node, visited: MutableSet<Node>) {
        for (predecessor in node.predecessors()) {
            if (predecessor !in visited) {
                visited.add(predecessor)
                scan(predecessor, visited)
            }
        }

        when (node) {
            is AddNode -> processNode(node)
            is DivNode -> processNode(node)
            is ModNode -> processNode(node)
            is MulNode -> processNode(node)
            is SubNode -> processNode(node)
            is Block -> processNode(node)
            is ConstIntNode -> processNode(node)
            is Phi -> processNode(node)
            is ProjNode -> processNode(node)
            is ReturnNode -> processNode(node)
            is StartNode -> processNode(node)
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