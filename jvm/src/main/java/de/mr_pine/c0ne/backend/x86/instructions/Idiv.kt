package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc

class Idiv(val value: Argument.RegMem) : Instruction("IDIV", value) {
    context(alloc: NextGenSimpleX86RegAlloc)
    override fun concretize(): Instruction {
        return Idiv(value.concretize() as Argument.RegMem)
    }
}
