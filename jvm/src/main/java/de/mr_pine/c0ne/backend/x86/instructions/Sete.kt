package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

class Sete(target: Argument.RegMem) : SetInsn("E", target) {
    context(alloc: X86RegAlloc)
    override fun concretize(): Instruction {
        return Sete(target.concretize() as Argument.RegMem)
    }
}
