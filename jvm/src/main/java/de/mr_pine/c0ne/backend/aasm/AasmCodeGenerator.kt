package de.mr_pine.c0ne.backend.aasm

import de.mr_pine.c0ne.backend.CodeGenerator
import de.mr_pine.c0ne.ir.left
import de.mr_pine.c0ne.ir.result
import de.mr_pine.c0ne.ir.right
import edu.kit.kastel.vads.compiler.ir.node.*
import edu.kit.kastel.vads.compiler.ir.util.NodeSupport


class AasmCodeGenerator : CodeGenerator<VirtualRegister, AasmRegisterAllocator.AasmRegisterAllocation> {
    companion object {
        private const val INDENTATION = "  "
    }

    var indentationLevel = 0
    fun StringBuilder.indent(): StringBuilder = append(INDENTATION.repeat(indentationLevel))

    override fun getAllocator() = AasmRegisterAllocator()

    override fun postprocess(generation: String) = generation.toByteArray()

    context(builder: StringBuilder)
    override fun prologue() {
    }

    context(builder: StringBuilder, registers: AasmRegisterAllocator.AasmRegisterAllocation)
    override fun functionPrologue(name: String) {
        builder.appendLine("function $name {")
        indentationLevel++
    }

    context(builder: StringBuilder, registers: AasmRegisterAllocator.AasmRegisterAllocation)
    override fun functionEpilogue(name: String) {
        builder.appendLine("}")
        indentationLevel--
    }

    context(builder: StringBuilder, registers: AasmRegisterAllocator.AasmRegisterAllocation)
    override fun processNode(node: AddNode) {
        processBinaryOp(node, "add")
    }

    context(builder: StringBuilder, registers: AasmRegisterAllocator.AasmRegisterAllocation)
    override fun processNode(node: SubNode) {
        processBinaryOp(node, "sub")
    }

    context(builder: StringBuilder, registers: AasmRegisterAllocator.AasmRegisterAllocation)
    override fun processNode(node: DivNode) {
        processBinaryOp(node, "div")
    }

    context(builder: StringBuilder, registers: AasmRegisterAllocator.AasmRegisterAllocation)
    override fun processNode(node: ModNode) {
        processBinaryOp(node, "mod")
    }

    context(builder: StringBuilder, registers: AasmRegisterAllocator.AasmRegisterAllocation)
    override fun processNode(node: MulNode) {
        processBinaryOp(node, "mul")
    }

    context(builder: StringBuilder, registers: AasmRegisterAllocator.AasmRegisterAllocation)
    fun processBinaryOp(node: BinaryOperationNode, opcode: String) {
        builder.indent()
            .append(registers[node])
            .append(" = ")
            .append(opcode)
            .append(" ")
            .append(node.left.registerOrConstValue)
            .append(" ")
            .append(node.right.registerOrConstValue)
            .appendLine()
    }

    context(builder: StringBuilder, registers: AasmRegisterAllocator.AasmRegisterAllocation)
    override fun processNode(node: ConstIntNode) {
        builder.indent()
            .append(registers[node])
            .append(" = const ${node.value()}")
            .appendLine()
    }

    context(builder: StringBuilder, registers: AasmRegisterAllocator.AasmRegisterAllocation)
    override fun processNode(node: Block) {
    }

    context(builder: StringBuilder, registers: AasmRegisterAllocator.AasmRegisterAllocation)
    override fun processNode(node: ProjNode) {
    }

    context(builder: StringBuilder, registers: AasmRegisterAllocator.AasmRegisterAllocation)
    override fun processNode(node: ReturnNode) {
        builder.indent()
            .append("ret ")
            .append(node.result.registerOrConstValue)
            .appendLine()
    }

    context(builder: StringBuilder, registers: AasmRegisterAllocator.AasmRegisterAllocation)
    override fun processNode(node: StartNode) {
    }

    context(registers: AasmRegisterAllocator.AasmRegisterAllocation)
    val Node.registerOrConstValue
        get() = if (this is ConstIntNode) value() else registers[this]
}