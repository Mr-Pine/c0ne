package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

class Je(target: Label) : Instruction("JE ${target.mnemonic}") {
    context(alloc: X86RegAlloc)
    override fun concretize() = this
}
