package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

class Setg(target: Argument.RegMem) : SetInsn("G", target) {
    context(alloc: X86RegAlloc)
    override fun concretize(): Instruction {
        return Setg(target.concretize() as Argument.RegMem)
    }
}
