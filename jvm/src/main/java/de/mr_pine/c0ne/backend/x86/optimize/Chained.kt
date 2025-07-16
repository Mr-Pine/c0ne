package de.mr_pine.c0ne.backend.x86.optimize

import de.mr_pine.c0ne.backend.x86.instructions.Instruction

data class Chained(val optimizations: List<PeepholeOptimization>): PeepholeOptimization {
    constructor(vararg optimizations: PeepholeOptimization): this(optimizations.toList())
    override fun optimize(instructions: MutableList<Instruction>) {
        for (optimization in optimizations) {
            optimization.optimize(instructions)
        }
    }

}
