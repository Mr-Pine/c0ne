package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

class Pop(val value: Argument) : Instruction("POP", value) {
    context(alloc: X86RegAlloc)
    override fun concretize(): Instruction {
        return Pop(value.concretize() as Argument.RegMem)
    }

    override fun render(size: Int): String {
        return "$mnemonic ${value.render(8)}"
    }
}
