package de.mr_pine.c0ne.backend.x86

import de.mr_pine.c0ne.backend.Schedule
import de.mr_pine.c0ne.backend.x86.instructions.*
import de.mr_pine.c0ne.ir.IrGraph
import de.mr_pine.c0ne.ir.node.*
import de.mr_pine.c0ne.ir.visitor.SSAVisitor
import kotlin.io.path.absolutePathString
import kotlin.io.path.createTempDirectory
import kotlin.io.path.writeText

class NextGenX86CodeGenerator(irGraphs: List<IrGraph>) {
    val schedules = irGraphs.map(::Schedule)
    val abstractInstructions =
        irGraphs.zip(schedules).map { (irGraph, schedule) -> AbstractCodegen(irGraph, schedule).abstractInstructions }
    val regAllocs = schedules.zip(irGraphs).map { (schedule, irGraph) ->
        NextGenSimpleX86RegAlloc(
            irGraph.startBlock, schedule
        )
    }
    val concreteInstructions =
        abstractInstructions.zip(regAllocs) { abstractInstructions, regAlloc -> with(regAlloc) { abstractInstructions.map { it.concretize() } } }

    val prefix = """
        .intel_syntax noprefix
        .global $ENTRY_NAME
        .text
        
        $ENTRY_NAME:
        call main
        push RAX
        call flush
        pop RAX
        mov EDI, EAX
        mov EAX, 0x3c
        syscall
        
        .extern putchar
        .global print
        print:
            call putchar
            mov RAX, 0
            ret
        
        .extern getchar
        .global read
        read = getchar
        
        .extern fflush
        .global flush
        flush:
            mov RDI, 0
            call fflush
            mov RAX, 0
            ret
    """.trimIndent() + "\n\n"

    fun generateAssembly(): String {
        val functionAssembly = concreteInstructions.map { it.joinToString(separator = "\n") { it.render() } }
        return prefix + functionAssembly.joinToString("\n\n")
    }

    companion object {
        const val ENTRY_NAME = "_c0ne_main_ae6b3e77fd3ff2765aa254f7e7d5cf0a"
        fun postprocess(generation: String): ByteArray {
            val tmpdir = createTempDirectory("c0ne")
            val input = tmpdir.resolve("input.s").apply { writeText(generation + "\n") }
            val output = tmpdir.resolve("c0ne_out")
            val assembler = ProcessBuilder(
                "gcc", input.absolutePathString(), "-g", "-o", output.absolutePathString(), "-Wl,-e$ENTRY_NAME"
            ).start()
            if (assembler.waitFor() != 0) {
                throw Exception("gcc assembly failed: ${assembler.errorStream.readAllBytes().decodeToString()}")
            }
            return output.toFile().readBytes()
        }
    }

    fun generate(): ByteArray {
        return postprocess(generateAssembly())
    }

    class AbstractCodegen(irGraph: IrGraph, schedule: Schedule) {

        companion object {

            // rax, rdi, rsi, rdx, rcx, r8, r9, r10, r11
            val callerSaved = listOf(
                X86Register.RealRegister.RAX,
                X86Register.RealRegister.RDI,
                X86Register.RealRegister.RSI,
                X86Register.RealRegister.RDX,
                X86Register.RealRegister.RCX,
                X86Register.RealRegister.R8,
                X86Register.RealRegister.R9,
                X86Register.RealRegister.R10,
                X86Register.RealRegister.R11
            )

            // rbx, rsp, rbp, r12, r13, r14
            val calleeSaved = listOf(
                X86Register.RealRegister.RBX,
                X86Register.RealRegister.R12,
                X86Register.RealRegister.R13,
                X86Register.RealRegister.R14
            )

            // rdi, rsi, rdx, rcx, r8, r9
            val arguments = sequenceOf(
                X86Register.RealRegister.RDI,
                X86Register.RealRegister.RSI,
                X86Register.RealRegister.RDX,
                X86Register.RealRegister.RCX,
                X86Register.RealRegister.R8,
                X86Register.RealRegister.R9
            ).map { Argument.RegMem.Register.RealRegister(it) } + generateSequence(Argument.RegMem.StackOverflowSlot(-16)) {
                Argument.RegMem.StackOverflowSlot(
                    it.offset - 8
                )
            }
        }

        val abstractInstructions = buildList {
            val visitor = AbstractCodegenVisitor(
                this,
                irGraph,
                schedule.blockOrder.first(),
                isInSchedule = { node, block -> node in schedule.blockSchedules[block]!!.nodeOrder })
            for (block in schedule.blockOrder) {
                block.accept(visitor)
                for (node in schedule.blockSchedules[block]?.nodeOrder ?: emptyList()) {
                    node.accept(visitor)
                }
            }
        }

        inner class AbstractCodegenVisitor(
            val instructionList: MutableList<Instruction>,
            val irGraph: IrGraph,
            private var currentBlock: Block,
            private val isInSchedule: (Node, Block) -> Boolean
        ) : SSAVisitor<Unit> {

            private fun visitNormalBinop(
                node: BinaryOperationNode, instructionConstructor: (Argument.RegMem.Register, Argument) -> Instruction
            ) {
                val arg1 = Argument.NodeValue(node.left)
                val arg2 = Argument.NodeValue(node.right)

                val target = Argument.NodeValue(node)
                val targetReg = Argument.RegMem.Register.RegisterFor(target)

                instructionList.add(Mov(targetReg, arg1))
                instructionList.add(instructionConstructor(targetReg, arg2))
                instructionList.add(Mov(target, targetReg))
            }

            override fun visit(node: AddNode) {
                visitNormalBinop(node, ::Add)
            }

            private fun visitShift(node: BinaryOperationNode, isLeftShift: Boolean) {
                val value = Argument.NodeValue(node.left)
                val shift = Argument.NodeValue(node.right)

                val target = Argument.NodeValue(node)
                val targetReg = Argument.RegMem.Register.RegisterFor(target)

                instructionList.add(Mov(targetReg, value))
                val insn = run {
                    val shiftReg = Argument.RegMem.Register.EcxOf(shift)
                    instructionList.add(Mov(shiftReg, shift))
                    if (isLeftShift) {
                        Sal(targetReg, shiftReg)
                    } else {
                        Sar(targetReg, shiftReg)
                    }
                }
                instructionList.add(insn)
                instructionList.add(Mov(target, targetReg))
            }

            override fun visit(node: ArithmeticLeftShiftNode) {
                visitShift(node, true)
            }

            override fun visit(node: ArithmeticRightShiftNode) {
                visitShift(node, false)
            }

            override fun visit(node: BitwiseAndNode) {
                visitNormalBinop(node, ::And)
            }

            override fun visit(node: BitwiseNotNode) {
                val value = Argument.NodeValue(node.value)

                val target = Argument.NodeValue(node)
                val targetReg = Argument.RegMem.Register.RegisterFor(target)
                instructionList.add(Mov(targetReg, value))
                instructionList.add(Not(targetReg))
                instructionList.add(Mov(target, targetReg))
            }

            override fun visit(node: BitwiseOrNode) {
                visitNormalBinop(node, ::Or)
            }

            override fun visit(node: Block) {
                currentBlock = node
                instructionList.add(Label(node))
            }

            override fun visit(node: ConstBoolNode) {
            }

            override fun visit(node: ConstIntNode) {
            }

            fun visitIdiv(node: BinaryOperationNode, targetRegister: X86Register.RealRegister) {
                val left = Argument.NodeValue(node.left)
                val right = Argument.NodeValue(node.right)

                val rightRegMem = Argument.RegMem.RegMemFor(right)
                instructionList.add(Mov(rightRegMem, right))

                val target = Argument.NodeValue(node)

                instructionList.add(Mov(Argument.RegMem.Register.RealRegister(X86Register.RealRegister.RAX), left))
                instructionList.add(Cdq())
                instructionList.add(Idiv(rightRegMem))
                instructionList.add(Mov(target, Argument.RegMem.Register.RealRegister(targetRegister)))
            }

            override fun visit(node: DivNode) {
                visitIdiv(node, X86Register.RealRegister.RAX)
            }

            private fun visitComparisonNode(
                node: BinaryOperationNode,
                setConstructor: (Argument.RegMem) -> SetInsn,
                size: Int = 4,
            ) {
                val left = Argument.NodeValue(node.left)
                val right = Argument.NodeValue(node.right)

                val leftReg = Argument.RegMem.Register.RegisterFor(left)
                instructionList.add(Mov(leftReg, left))

                val target = Argument.NodeValue(node)

                instructionList.add(Cmp(leftReg, right, size))
                instructionList.add(setConstructor(target))
            }

            override fun visit(node: EqualsNode) {
                visitComparisonNode(node, ::Sete, node.size)
            }

            override fun visit(node: IfNode) {
                val (trueTarget, falseTarget) = irGraph.successors(node).map { irGraph.successors(it).first() }
                    .map { Label(it.block) }

                val condition = Argument.NodeValue(node.condition)
                val conditionRegMem = Argument.RegMem.RegMemFor(condition)
                instructionList.add(Mov(conditionRegMem, condition))

                instructionList.add(Cmp(conditionRegMem, Argument.Immediate(1), size = 1))
                instructionList.add(Je(trueTarget))
                instructionList.add(Jmp(falseTarget))
            }

            override fun visit(node: JumpNode) {
                val target = Label(irGraph.successors(node).first().block)
                instructionList.add(Jmp(target))
            }

            override fun visit(node: LessThanEqNode) {
                visitComparisonNode(node, ::Setle)
            }

            override fun visit(node: LessThanNode) {
                visitComparisonNode(node, ::Setl)
            }

            override fun visit(node: ModNode) {
                visitIdiv(node, X86Register.RealRegister.RDX)
            }

            override fun visit(node: MulNode) {
                visitNormalBinop(node, ::Imul)
            }

            override fun visit(node: Phi) {
                val predecessorIndex = node.block.predecessors().indexOfFirst { it.block == currentBlock }
                val predecessor = node[predecessorIndex]

                val value = Argument.NodeValue(predecessor)

                val target = Argument.NodeValue(node)
                val tmpReg = Argument.RegMem.Register.RegisterFor(target)
                instructionList.add(Mov(tmpReg, value))
                instructionList.add(Mov(target, tmpReg))
            }

            override fun visit(node: ProjNode) {
            }

            override fun visit(node: ReturnNode) {
                val value = Argument.NodeValue(node.result)
                instructionList.add(Mov(Argument.RegMem.Register.RealRegister(X86Register.RealRegister.RAX), value))

                instructionList.add(Leave())
                instructionList.add(Ret())
            }

            override fun visit(node: StartNode) {
                instructionList.add(
                    Enter(
                        node,
                        irGraph.successors(node)
                            .mapNotNull { (it as? ProjNode)?.takeIf { it.projectionInfo() is ProjNode.NamedParameterProjectionInfo } }
                            .map { it.takeIf { isInSchedule(it, currentBlock) } }
                            .map { it?.let { Argument.NodeValue(it) } })
                )
            }

            override fun visit(node: SubNode) {
                visitNormalBinop(node, ::Sub)
            }

            override fun visit(node: UndefNode) {
                throw IllegalStateException("Got UndefNode during codegen")
            }

            override fun visit(node: XorNode) {
                visitNormalBinop(node, ::Xor)
            }

            override fun visit(node: CallNode) {
                val returnTarget = Argument.NodeValue(node)
                instructionList.add(
                    Call(
                        node.target,
                        returnTarget,
                        node.arguments.map { Argument.NodeValue(it) })
                )
            }
        }
    }
}