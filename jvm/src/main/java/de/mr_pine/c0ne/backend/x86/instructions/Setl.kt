package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

class Setl(target: Argument.RegMem) : SetInsn("L", target) {
    context(alloc: X86RegAlloc)
    override fun concretize(): Instruction {
        return Setl(target.concretize() as Argument.RegMem)
    }
}
