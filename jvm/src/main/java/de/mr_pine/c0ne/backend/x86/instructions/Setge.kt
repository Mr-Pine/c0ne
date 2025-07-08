package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

class Setge(target: Argument.RegMem) : SetInsn("GE", target) {
    context(alloc: X86RegAlloc)
    override fun concretize(): Instruction {
        return Setge(target.concretize() as Argument.RegMem)
    }
}
