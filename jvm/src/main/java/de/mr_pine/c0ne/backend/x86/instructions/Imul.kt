package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

class Imul(val left: Argument, val right: Argument) : Instruction("IMUL", left, right) {

    context(alloc: X86RegAlloc)
    override fun concretize(): Instruction {
        return Imul(left.concretize(), right.concretize())
    }
}
