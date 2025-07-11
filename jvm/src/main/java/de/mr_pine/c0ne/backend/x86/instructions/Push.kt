package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

class Push(val value: Argument) : Instruction("PUSH", value) {
    context(alloc: X86RegAlloc)
    override fun concretize(): Instruction {
        return Push(value.concretize())
    }

    override fun render(size: Int): String {
        return "$mnemonic ${value.render(8)}"
    }
}
