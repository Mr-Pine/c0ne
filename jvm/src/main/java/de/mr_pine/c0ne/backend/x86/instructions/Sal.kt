package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

class Sal private constructor(val left: Argument, val right: Argument) : Instruction("SAL", left, right) {
    constructor(left: Argument.RegMem, right: Argument.Immediate): this(left as Argument, right as Argument)
    constructor(left: Argument.RegMem, right: Argument.RegMem.Register.EcxOf): this(left as Argument, right as Argument)

    context(alloc: X86RegAlloc)
    override fun concretize(): Instruction {
        return Sal(left.concretize(), right.concretize())
    }
}
