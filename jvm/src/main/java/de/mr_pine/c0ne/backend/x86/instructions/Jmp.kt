package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc

class Jmp(target: Label) : Instruction("JMP ${target.mnemonic}") {
    context(alloc: NextGenSimpleX86RegAlloc)
    override fun concretize() = this
}
