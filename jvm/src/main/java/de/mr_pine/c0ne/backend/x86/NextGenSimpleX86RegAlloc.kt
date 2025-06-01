package de.mr_pine.c0ne.backend.x86

import de.mr_pine.c0ne.backend.x86.instructions.Argument
import de.mr_pine.c0ne.backend.x86.instructions.Instruction

class NextGenSimpleX86RegAlloc(abstractInstructions: List<Instruction>) {
    private val toAllocate = abstractInstructions.flatMap { it.sources.toList() }.mapNotNull(Argument::nodeValue).toSet()
    private var allocatable = (X86Register.RealRegister.entries - listOf(
        X86Register.RealRegister.EAX,
        X86Register.RealRegister.EDX,
        X86Register.RealRegister.RBP,
        X86Register.RealRegister.RSP,
        X86Register.RealRegister.R15D
    )).map { Argument.RegMem.Register.RealRegister(it) }
        .asSequence() + generateSequence(Argument.RegMem.StackOverflowSlot(0)) { Argument.RegMem.StackOverflowSlot(it.index + 1) }

    private val allocation =
        allocatable.zip(Sequence { toAllocate.iterator() }).map { (reg, node) -> node to reg }.toMap()

    fun concretize(argument: Argument.NodeValue): Argument.RegMem {
        return allocation[argument.nodeValue] ?: error("No register allocated for $argument")
    }

    fun concretize(argument: Argument.RegMem.Register.RegisterFor): Argument.RegMem.Register.RealRegister {
        return Argument.RegMem.Register.RealRegister(X86Register.RealRegister.R15D)
    }

    fun concretize(argument: Argument.RegMem.RegMemFor): Argument {
        return argument.arg.concretize()
    }

    fun concretize(argument: Argument.RegMem.StackOverflowSlot) = argument
    fun concretize(argument: Argument.RegMem.Register.RealRegister) = argument
    fun concretize(argument: Argument.Immediate) = argument
    fun concretize(argument: Argument.RegMem.Register.EcxOf) = argument

}