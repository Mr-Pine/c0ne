package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc

class Setg(target: Argument.RegMem) : SetInsn("G", target) {
    context(alloc: NextGenSimpleX86RegAlloc)
    override fun concretize(): Instruction {
        return Setg(target.concretize() as Argument.RegMem)
    }
}
