package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

class And(val left: Argument, val right: Argument) : Instruction("AND", left, right) {

    context(alloc: X86RegAlloc)
    override fun concretize() = And(left.concretize(), right.concretize())
}
