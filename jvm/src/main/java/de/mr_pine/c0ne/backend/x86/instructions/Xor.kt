package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

class Xor(val left: Argument, val right: Argument) : Instruction("XOR", left, right) {

    context(alloc: X86RegAlloc)
    override fun concretize() = Xor(left.concretize(), right.concretize())
}
