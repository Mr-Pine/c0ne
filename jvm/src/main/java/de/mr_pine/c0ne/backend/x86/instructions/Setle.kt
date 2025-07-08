package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

class Setle(target: Argument.RegMem) : SetInsn("LE", target) {
    context(alloc: X86RegAlloc)
    override fun concretize(): Instruction {
        return Setle(target.concretize() as Argument.RegMem)
    }
}
