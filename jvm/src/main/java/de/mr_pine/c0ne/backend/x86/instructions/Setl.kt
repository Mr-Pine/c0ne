package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc

class Setl(target: Argument.RegMem) : SetInsn("L", target) {
    context(alloc: NextGenSimpleX86RegAlloc)
    override fun concretize(): Instruction {
        return Setl(target.concretize() as Argument.RegMem)
    }
}
