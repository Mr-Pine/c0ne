package de.mr_pine.c0ne.backend.x86

import de.mr_pine.c0ne.backend.Schedule
import de.mr_pine.c0ne.backend.x86.instructions.*
import de.mr_pine.c0ne.ir.IrGraph
import de.mr_pine.c0ne.ir.node.*
import de.mr_pine.c0ne.ir.visitor.SSAVisitor

class NextGenX86CodeGenerator(irGraph: IrGraph) {
    val abstractInstructions = AbstractCodegen(irGraph, Schedule(irGraph)).abstractInstructions
    val regAlloc = NextGenSimpleX86RegAlloc(abstractInstructions)
    val concreteInstructions = with(regAlloc) { abstractInstructions.map { it.concretize() } }

    class AbstractCodegen(irGraph: IrGraph, schedule: Schedule) {

        val abstractInstructions = buildList {
            val visitor = AbstractCodegenVisitor(this, irGraph, schedule.blockOrder.first())
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
            private var currentBlock: Block
        ) :
            SSAVisitor<Unit> {
            val nodeValues = mutableMapOf<Node, Argument>()

            private fun visitNormalBinop(
                node: BinaryOperationNode,
                instructionConstructor: (Argument.RegMem.Register, Argument) -> Instruction
            ) {
                val arg1 = nodeValues[node.left]!!
                val arg2 = nodeValues[node.right]!!

                val target = Argument.NodeValue(node)
                val targetReg = Argument.RegMem.Register.RegisterFor(target)

                instructionList.add(Mov(targetReg, arg1))
                instructionList.add(instructionConstructor(targetReg, arg2))
                instructionList.add(Mov(target, targetReg))

                nodeValues[node] = target
            }

            override fun visit(node: AddNode) {
                visitNormalBinop(node, ::Add)
            }

            private fun visitShift(node: BinaryOperationNode, isLeftShift: Boolean) {
                val value = nodeValues[node.left]!!
                val shift = nodeValues[node.right]!!

                val target = Argument.NodeValue(node)

                instructionList.add(Mov(target, value))
                val insn = if (shift is Argument.Immediate) {
                    if (isLeftShift) {
                        Sal(target, shift)
                    } else {
                        Sar(target, shift)
                    }
                } else {
                    val shiftReg = Argument.RegMem.Register.EcxOf(shift)
                    if (isLeftShift) {
                        Sal(target, shiftReg)
                    } else {
                        Sar(target, shiftReg)
                    }
                }
                instructionList.add(insn)
                instructionList.add(Mov(target, target))
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
                val value = nodeValues[node.value]!!

                val target = Argument.NodeValue(node)
                instructionList.add(Mov(target, value))
                instructionList.add(Not(target))
            }

            override fun visit(node: BitwiseOrNode) {
                visitNormalBinop(node, ::Or)
            }

            override fun visit(node: Block) {
                currentBlock = node
                instructionList.add(Label(node))
            }

            override fun visit(node: ConstBoolNode) {
                val target = Argument.NodeValue(node)
                instructionList.add(Mov(target, Argument.Immediate(if (node.value) 1 else 0)))
                nodeValues[node] = target
            }

            override fun visit(node: ConstIntNode) {
                val target = Argument.NodeValue(node)
                instructionList.add(Mov(target, Argument.Immediate(node.value)))
                nodeValues[node] = target
            }

            override fun visit(node: DivNode) {
                val left = nodeValues[node.left]!!
                val right = nodeValues[node.right]!!

                val rightRegMem = Argument.RegMem.RegMemFor(right)

                val target = Argument.NodeValue(node)

                instructionList.add(Mov(Argument.RegMem.Register.RealRegister(X86Register.RealRegister.EAX), left))
                instructionList.add(Cdq())
                instructionList.add(Idiv(rightRegMem))
                instructionList.add(Mov(target, Argument.RegMem.Register.RealRegister(X86Register.RealRegister.EAX)))
            }

            private fun visitComparisonNode(
                node: BinaryOperationNode,
                trueCmov: (Argument.RegMem, Argument) -> Instruction,
                falseCmov: (Argument.RegMem, Argument) -> Instruction
            ) {
                val left = nodeValues[node.left]!!
                val right = nodeValues[node.right]!!

                val leftRegMem = Argument.RegMem.RegMemFor(left)
                val target = Argument.NodeValue(node)

                instructionList.add(Cmp(leftRegMem, right))
                instructionList.add(trueCmov(target, Argument.Immediate(1)))
                instructionList.add(falseCmov(target, Argument.Immediate(0)))
            }

            override fun visit(node: EqualsNode) {
                visitComparisonNode(node, ::Cmove, ::Cmovne)
            }

            override fun visit(node: IfNode) {
                val (trueTarget, falseTarget) = irGraph.successors(node).map { irGraph.successors(it).first() }
                    .map { Label(it.block) }

                val condition = Argument.RegMem.RegMemFor(nodeValues[node.condition]!!)

                instructionList.add(Cmp(condition, Argument.Immediate(1)))
                instructionList.add(Je(trueTarget))
                instructionList.add(Jmp(falseTarget))
            }

            override fun visit(node: JumpNode) {
                val target = Label(irGraph.successors(node).first().block)
                instructionList.add(Jmp(target))
            }

            override fun visit(node: LessThanEqNode) {
                visitComparisonNode(node, ::Cmovle, ::Cmovg)
            }

            override fun visit(node: LessThanNode) {
                visitComparisonNode(node, ::Cmovl, ::Cmovge)
            }

            override fun visit(node: LogicalAndNode) {
                visitNormalBinop(node, ::And)
            }

            override fun visit(node: LogicalOrNode) {
                visitNormalBinop(node, ::Or)
            }

            override fun visit(node: ModNode) {
                val left = nodeValues[node.left]!!
                val right = nodeValues[node.right]!!

                val rightRegMem = Argument.RegMem.RegMemFor(right)

                val target = Argument.NodeValue(node)

                instructionList.add(Mov(Argument.RegMem.Register.RealRegister(X86Register.RealRegister.EAX), left))
                instructionList.add(Cdq())
                instructionList.add(Idiv(rightRegMem))
                instructionList.add(Mov(target, Argument.RegMem.Register.RealRegister(X86Register.RealRegister.EDX)))
            }

            override fun visit(node: MulNode) {
                visitNormalBinop(node, ::Imul)
            }

            override fun visit(node: NotNode) {
                val value = nodeValues[node.value]!!

                val target = Argument.NodeValue(node)
                instructionList.add(Mov(target, value))
                instructionList.add(Not(target))
            }

            override fun visit(node: Phi) {
                val predecessorIndex = node.block.predecessors().indexOfFirst { it.block == currentBlock }
                val predecessor = node.predecessors()[predecessorIndex]

                val value = Argument.NodeValue(predecessor)

                val target = Argument.NodeValue(node)
                instructionList.add(Mov(target, value))
            }

            override fun visit(node: ProjNode) {
            }

            override fun visit(node: ReturnNode) {
                val value = Argument.NodeValue(node.result)
                instructionList.add(Mov(Argument.RegMem.Register.RealRegister(X86Register.RealRegister.EAX), value))
            }

            override fun visit(node: StartNode) {

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
        }
    }
}