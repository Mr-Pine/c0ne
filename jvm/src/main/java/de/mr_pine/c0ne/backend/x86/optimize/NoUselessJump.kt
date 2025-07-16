package de.mr_pine.c0ne.backend.x86.optimize

import de.mr_pine.c0ne.backend.x86.instructions.Instruction
import de.mr_pine.c0ne.backend.x86.instructions.Jmp

object NoUselessJump : PeepholeOptimization {
    override fun optimize(instructions: MutableList<Instruction>) {
        val keep = instructions.filterIndexed { index, instruction ->
            if (instruction !is Jmp) return@filterIndexed true
            val next = instructions.getOrNull(index + 1)
            instruction.target != next
        }
        instructions.clear()
        instructions.addAll(keep)
    }
}