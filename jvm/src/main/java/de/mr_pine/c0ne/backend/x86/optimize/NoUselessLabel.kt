package de.mr_pine.c0ne.backend.x86.optimize

import de.mr_pine.c0ne.backend.x86.instructions.Instruction
import de.mr_pine.c0ne.backend.x86.instructions.Jmp
import de.mr_pine.c0ne.backend.x86.instructions.Jmpcc
import de.mr_pine.c0ne.backend.x86.instructions.Label

object NoUselessLabel : PeepholeOptimization {
    override fun optimize(instructions: MutableList<Instruction>) {
        instructions.removeIf { label -> label is Label && instructions.none { it is Jmp && it.target == label || it is Jmpcc && it.target == label } }
    }
}