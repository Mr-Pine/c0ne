package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc

class Not(val value: Argument.RegMem) : Instruction("NOT", value) {
    context(alloc: NextGenSimpleX86RegAlloc)
    override fun concretize(): Instruction {
        return Not(value.concretize() as Argument.RegMem)
    }
}
