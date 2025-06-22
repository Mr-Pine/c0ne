package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc

class Imul private constructor(val left: Argument, val right: Argument) : Instruction("IMUL", left, right) {
    constructor(left: Argument.RegMem.Register, right: Argument): this(left as Argument, right)

    context(alloc: NextGenSimpleX86RegAlloc)
    override fun concretize(): Instruction {
        return Imul(left.concretize(), right.concretize())
    }
}
