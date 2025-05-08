package de.mr_pine.c0ne.backend.x86

import de.mr_pine.c0ne.backend.CodeGenerator
import de.mr_pine.c0ne.backend.RegisterAllocator
import de.mr_pine.c0ne.ir.result
import edu.kit.kastel.vads.compiler.ir.node.*
import edu.kit.kastel.vads.compiler.ir.util.NodeSupport
import kotlin.io.path.absolutePathString
import kotlin.io.path.createTempDirectory
import kotlin.io.path.writeText

// private typealias Allocation = X86ColoringRegisterAllocator.X86ColoringRegisterAllocation
private typealias Allocation = X86StraightLineRegisterAllocator.X86StraightLineRegisterAllocation

class X86CodeGenerator : CodeGenerator<X86Register, Allocation> {

    override fun getAllocator(): RegisterAllocator<X86Register, Allocation> = X86StraightLineRegisterAllocator()
    // override fun getAllocator(): RegisterAllocator<X86Register, Allocation> = X86ColoringRegisterAllocator()

    override fun postprocess(generation: String): ByteArray {
        val tmpdir = createTempDirectory("c0ne")
        val input = tmpdir.resolve("input.s").apply { writeText(generation) }
        val output = tmpdir.resolve("c0ne_out")
        val assembler = ProcessBuilder("gcc", input.absolutePathString(), "-o", output.absolutePathString()).start()
        if (assembler.waitFor() != 0) {
            throw Exception("gcc assembly failed: ${assembler.errorStream.readAllBytes().decodeToString()}")
        }
        return output.toFile().readBytes()
    }

    context(builder: StringBuilder) override fun prologue() {
        builder.appendLine(".intel_syntax noprefix").appendLine(".global main").appendLine(".text").appendLine()
            .appendLine("main:").appendLine("call _main").appendLine()
            .appendLine("mov ${X86Register.RealRegister.EDI}, ${X86Register.RealRegister.EAX}")
            .appendLine("mov ${X86Register.RealRegister.EAX}, 0x3C").appendLine("syscall").appendLine()
    }

    context(builder: StringBuilder, registers: Allocation) override fun functionPrologue(name: String) {
        if (name == "main") {
            functionPrologue("_main")
            return
        }
        builder.appendLine(".global $name").appendLine("$name:")
        processInstruction(Instruction.ENTER, registers.overflowCount * 4, 0)
    }

    context(builder: StringBuilder, registers: Allocation) override fun functionEpilogue(name: String) {
        if (name == "main") {
            functionEpilogue("_main")
            return
        }
        processInstruction(Instruction.LEAVE)
        builder.appendLine("# end of $name")
    }

    context(builder: StringBuilder, registers: Allocation) override fun processNode(node: AddNode) {
        processInstruction(
            Instruction.MOV, registers[node], registers[NodeSupport.predecessorSkipProj(node, AddNode.LEFT)]
        )
        processInstruction(
            Instruction.ADD, registers[node], registers[NodeSupport.predecessorSkipProj(node, AddNode.RIGHT)]
        )
    }

    context(builder: StringBuilder, registers: Allocation) override fun processNode(node: SubNode) {
        processInstruction(
            Instruction.MOV, registers[node], registers[NodeSupport.predecessorSkipProj(node, AddNode.LEFT)]
        )
        processInstruction(
            Instruction.SUB, registers[node], registers[NodeSupport.predecessorSkipProj(node, AddNode.RIGHT)]
        )
    }

    context(builder: StringBuilder, registers: Allocation) override fun processNode(node: DivNode) {
        processInstruction(
            Instruction.MOV,
            X86Register.RealRegister.EAX,
            registers[NodeSupport.predecessorSkipProj(node, DivNode.LEFT)]
        )
        processInstruction(Instruction.CDQ)
        processInstruction(Instruction.IDIV, registers[NodeSupport.predecessorSkipProj(node, DivNode.RIGHT)])
        processInstruction(Instruction.MOV, registers[node], X86Register.RealRegister.EAX)
    }

    context(builder: StringBuilder, registers: Allocation) override fun processNode(node: ModNode) {
        processInstruction(
            Instruction.MOV,
            X86Register.RealRegister.EAX,
            registers[NodeSupport.predecessorSkipProj(node, DivNode.LEFT)]
        )
        processInstruction(Instruction.CDQ)
        processInstruction(Instruction.IDIV, registers[NodeSupport.predecessorSkipProj(node, DivNode.RIGHT)])
        processInstruction(Instruction.MOV, registers[node], X86Register.RealRegister.EDX)
    }

    context(builder: StringBuilder, registers: Allocation) override fun processNode(node: MulNode) {
        processInstruction(
            Instruction.MOV, registers[node], registers[NodeSupport.predecessorSkipProj(node, MulNode.LEFT)]
        )
        processInstruction(
            Instruction.IMUL, registers[node], registers[NodeSupport.predecessorSkipProj(node, MulNode.RIGHT)]
        )
    }

    context(builder: StringBuilder, registers: Allocation) override fun processNode(node: ConstIntNode) {
        processInstruction(Instruction.MOV, registers[node], node.value())
    }

    context(builder: StringBuilder, registers: Allocation) override fun processNode(node: Block) {
    }

    context(builder: StringBuilder, registers: Allocation) override fun processNode(node: ProjNode) {
    }

    context(builder: StringBuilder, registers: Allocation) override fun processNode(node: ReturnNode) {
        processInstruction(Instruction.MOV, X86Register.RealRegister.EAX, registers[node.result])
        processInstruction(Instruction.LEAVE)
        processInstruction(Instruction.RET)
    }

    context(builder: StringBuilder, registers: Allocation) override fun processNode(node: StartNode) {
    }

    context(builder: StringBuilder) private fun processInstruction(instruction: Instruction, vararg operands: Any) {
        if (operands.isNotEmpty() && operands[0] is X86Register.OverflowSlot) {
            processInstruction(Instruction.MOV, X86Register.RealRegister.R15D, operands[0])
            processInstruction(instruction, *(listOf(X86Register.RealRegister.R15D) + operands.drop(1)).toTypedArray())
            builder.append(Instruction.MOV.name).append(" ")
                .append(listOf(operands[0], X86Register.RealRegister.R15D).joinToString(", ")).appendLine()
        } else {
            builder.append(instruction.name).append(" ").append(operands.joinToString(", ")).appendLine()
        }
    }

    private enum class Instruction {
        MOV, ADD, SUB, IDIV, CDQ, IMUL, ENTER, LEAVE, RET
    }
}