package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

class Add(val left: Argument, val right: Argument) : Instruction("ADD", left, right) {

    context(alloc: X86RegAlloc)
    override fun concretize() = Add(left.concretize(), right.concretize())
}
