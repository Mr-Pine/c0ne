package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc

class Mov private constructor(val left: Argument, val right: Argument) : Instruction("MOV", left, right) {
    constructor(left: Argument.RegMem, right: Argument): this(left as Argument, right)

    context(alloc: NextGenSimpleX86RegAlloc)
    override fun concretize(): Instruction {
        val leftConcrete = left.concretize()
        val rightConcrete = right.concretize()
        if (leftConcrete is Argument.RegMem.StackOverflowSlot && rightConcrete is Argument.RegMem.StackOverflowSlot) {
            error("Impossible move")
        }
        return Mov(leftConcrete, rightConcrete)
    }
}
