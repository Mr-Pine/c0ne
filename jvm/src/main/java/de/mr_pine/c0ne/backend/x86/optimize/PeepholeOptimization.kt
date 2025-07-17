package de.mr_pine.c0ne.backend.x86.optimize

import de.mr_pine.c0ne.backend.x86.instructions.Instruction

interface PeepholeOptimization {
    fun optimize(instructions: MutableList<Instruction>)
}