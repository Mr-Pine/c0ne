package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

class Setle(target: Argument.RegMem) : Setcc("LE", target) {
    context(alloc: X86RegAlloc)
    override fun concretize(): Instruction {
        return Setle(target.concretize() as Argument.RegMem)
    }
}
