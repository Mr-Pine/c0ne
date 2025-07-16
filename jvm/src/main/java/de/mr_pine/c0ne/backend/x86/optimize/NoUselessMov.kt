package de.mr_pine.c0ne.backend.x86.optimize

import de.mr_pine.c0ne.backend.x86.instructions.Instruction
import de.mr_pine.c0ne.backend.x86.instructions.Mov

object NoUselessMov : PeepholeOptimization {
    override fun optimize(instructions: MutableList<Instruction>) {
        instructions.removeAll { it is Mov && it.left == it.right }
    }
}