package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc

class Cmovge(target: Argument.RegMem, source: Argument) : Cmov("GE", target, source) {
    context(alloc: NextGenSimpleX86RegAlloc)
    override fun concretize(): Instruction {
        return Cmovge(target.concretize() as Argument.RegMem, source.concretize())
    }
}
