package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

class Or(val left: Argument, val right: Argument) : Instruction("OR", left, right) {

    context(alloc: X86RegAlloc)
    override fun concretize(): Instruction {
        return Or(left.concretize(), right.concretize())
    }
}
