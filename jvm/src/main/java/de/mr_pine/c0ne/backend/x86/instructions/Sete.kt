package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc

class Sete(target: Argument.RegMem) : SetInsn("E", target) {
    context(alloc: NextGenSimpleX86RegAlloc)
    override fun concretize(): Instruction {
        return Sete(target.concretize() as Argument.RegMem)
    }
}
