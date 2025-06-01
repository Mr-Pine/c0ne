package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc

class Cmovle(target: Argument.RegMem, source: Argument) : Cmov("LE", target, source) {
    context(alloc: NextGenSimpleX86RegAlloc)
    override fun concretize(): Instruction {
        return Cmovle(target.concretize() as Argument.RegMem, source.concretize())
    }
}
