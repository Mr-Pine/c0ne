package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

class Sub(val left: Argument, val right: Argument) : Instruction("SUB", left, right) {

    context(alloc: X86RegAlloc)
    override fun concretize() = Sub(left.concretize(), right.concretize())
}
