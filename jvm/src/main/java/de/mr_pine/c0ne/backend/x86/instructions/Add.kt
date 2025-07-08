package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

class Add private constructor(val left: Argument, val right: Argument) : Instruction("ADD", left, right) {
    constructor(left: Argument.RegMem.Register, right: Argument): this(left as Argument, right)

    context(alloc: X86RegAlloc)
    override fun concretize() = Add(left.concretize(), right.concretize())
}
