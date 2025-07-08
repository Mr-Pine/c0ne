package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

class Sar private constructor(val left: Argument, val right: Argument) : Instruction("SAR", left, right) {
    constructor(left: Argument.RegMem, right: Argument.Immediate): this(left as Argument, right as Argument)
    constructor(left: Argument.RegMem, right: Argument.RegMem.Register.EcxOf): this(left as Argument, right as Argument)

    context(alloc: X86RegAlloc)
    override fun concretize(): Instruction {
        return Sar(left.concretize(), right.concretize())
    }
}
