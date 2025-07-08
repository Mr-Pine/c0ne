package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

class Or private constructor(val left: Argument, val right: Argument) : Instruction("OR", left, right) {
    constructor(left: Argument.RegMem.Register, right: Argument): this(left as Argument, right)

    context(alloc: X86RegAlloc)
    override fun concretize(): Instruction {
        return Or(left.concretize(), right.concretize())
    }
}
