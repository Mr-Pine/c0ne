package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

class Idiv(val value: Argument.RegMem) : Instruction("IDIV", value) {
    context(alloc: X86RegAlloc)
    override fun concretize(): Instruction {
        return Idiv(value.concretize() as Argument.RegMem)
    }
}
