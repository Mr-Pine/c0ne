package de.mr_pine.c0ne.backend.x86.optimize

import de.mr_pine.c0ne.backend.x86.instructions.*

object NoUselessSetCmpChains : PeepholeOptimization {
    override fun optimize(instructions: MutableList<Instruction>) {
        val processed = buildList {
            var i = 0
            while (i < instructions.size) {
                val current = instructions[i]
                val next = instructions.getOrNull(i + 1)
                val nextNext = instructions.getOrNull(i + 2)
                if (current !is Setcc || next !is Cmp || nextNext !is Jmpcc) {
                    add(current)
                    i++
                    continue
                }

                val jmpConstructor = when (current) {
                    is Sete -> ::Je
                    is Setg -> ::Jg
                    is Setge -> ::Jge
                    is Setl -> ::Jl
                    is Setle -> ::Jle
                    is Setne -> ::Jne
                }

                add(jmpConstructor(nextNext.target))
                i += 3
            }

        }
        instructions.clear()
        instructions.addAll(processed)
    }
}