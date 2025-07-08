package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

class Setne(target: Argument.RegMem) : SetInsn("NE", target) {
    context(alloc: X86RegAlloc)
    override fun concretize(): Instruction {
        return Setne(target.concretize() as Argument.RegMem)
    }
}
