package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc
import de.mr_pine.c0ne.backend.x86.X86Register

class Mov private constructor(val left: Argument, val right: Argument) : Instruction("MOV", left, right) {
    constructor(left: Argument.RegMem, right: Argument): this(left as Argument, right)
    constructor(left: Argument, right: Argument.RegMem): this(left, right as Argument)
    constructor(left: Argument.RegMem, right: Argument.RegMem): this(left as Argument, right as Argument)

    context(alloc: NextGenSimpleX86RegAlloc)
    override fun concretize(): Instruction {
        val leftConcrete = left.concretize()
        val rightConcrete = right.concretize()
        if (leftConcrete is Argument.RegMem.StackOverflowSlot && rightConcrete is Argument.RegMem.StackOverflowSlot) {
            if (leftConcrete == rightConcrete) {
                return Mov(Argument.RegMem.Register.RealRegister(X86Register.RealRegister.R15), Argument.RegMem.Register.RealRegister(X86Register.RealRegister.R15))
            }
            error("Impossible move")
        }
        return Mov(leftConcrete, rightConcrete)
    }
}
