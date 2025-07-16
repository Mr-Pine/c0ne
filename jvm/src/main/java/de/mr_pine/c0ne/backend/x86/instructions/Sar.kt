package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

class Sar(val left: Argument, val right: Argument) : Instruction("SAR", left, right) {

    context(alloc: X86RegAlloc)
    override fun concretize(): Instruction {
        return Sar(left.concretize(), right.concretize())
    }
}
