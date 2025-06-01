package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc

class Cmove(target: Argument.RegMem, source: Argument) : Cmov("E", target, source) {
    context(alloc: NextGenSimpleX86RegAlloc)
    override fun concretize(): Instruction {
        return Cmove(target.concretize() as Argument.RegMem, source.concretize())
    }
}
