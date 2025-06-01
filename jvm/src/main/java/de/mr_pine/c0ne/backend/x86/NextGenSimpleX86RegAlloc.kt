package de.mr_pine.c0ne.backend.x86

import de.mr_pine.c0ne.backend.x86.instructions.Argument
import de.mr_pine.c0ne.backend.x86.instructions.Instruction

class NextGenSimpleX86RegAlloc(abstractInstructions: List<Instruction>) {
    private val toAllocate =
        abstractInstructions.flatMap { it.sources.toList() }.mapNotNull(Argument::nodeValue).toSet()
    private var allocatable = (X86Register.RealRegister.entries - listOf(
        X86Register.RealRegister.RAX,
        X86Register.RealRegister.RDX,
        X86Register.RealRegister.RBP,
        X86Register.RealRegister.RSP,
        X86Register.RealRegister.R15
    )).map { Argument.RegMem.Register.RealRegister(it) }
        .asSequence() + generateSequence(Argument.RegMem.StackOverflowSlot(0)) { Argument.RegMem.StackOverflowSlot(it.index + 1) }

    private val allocation =
        allocatable.zip(Sequence { toAllocate.iterator() }).map { (reg, node) -> node to reg }.toMap()

    val overflowCount
        get() = allocation.values.mapNotNull { it as? Argument.RegMem.StackOverflowSlot }.maxOfOrNull { it.index } ?: 0

    fun concretize(argument: Argument.NodeValue): Argument.RegMem {
        return allocation[argument.nodeValue] ?: error("No register allocated for $argument")
    }

    fun concretize(argument: Argument.RegMem.Register.RegisterFor): Argument.RegMem.Register.RealRegister {
        return Argument.RegMem.Register.RealRegister(X86Register.RealRegister.R15)
    }

    fun concretize(argument: Argument.RegMem.RegMemFor): Argument {
        return argument.arg.concretize()
    }

    fun concretize(argument: Argument.RegMem.StackOverflowSlot) = argument
    fun concretize(argument: Argument.RegMem.Register.RealRegister) = argument
    fun concretize(argument: Argument.Immediate) = argument
    fun concretize(argument: Argument.RegMem.Register.EcxOf) = Argument.RegMem.Register.RealRegister(X86Register.RealRegister.RCX)

}