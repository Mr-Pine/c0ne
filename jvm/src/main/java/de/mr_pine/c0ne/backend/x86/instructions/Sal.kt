package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

class Sal(val left: Argument, val right: Argument) : Instruction("SAL", left, right) {

    context(alloc: X86RegAlloc)
    override fun concretize(): Instruction {
        return Sal(left.concretize(), right.concretize())
    }
}
