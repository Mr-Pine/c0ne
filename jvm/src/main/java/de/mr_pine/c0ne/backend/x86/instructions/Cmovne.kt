package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc

class Cmovne(target: Argument.RegMem, source: Argument) : Cmov("NE", target, source) {
    context(alloc: NextGenSimpleX86RegAlloc)
    override fun concretize(): Instruction {
        return Cmovne(target.concretize() as Argument.RegMem, source.concretize())
    }
}
