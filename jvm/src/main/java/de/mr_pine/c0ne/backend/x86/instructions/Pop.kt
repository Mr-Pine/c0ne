package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc

class Pop(val value: Argument) : Instruction("POP", value) {
    context(alloc: NextGenSimpleX86RegAlloc)
    override fun concretize(): Instruction {
        return Pop(value.concretize() as Argument.RegMem)
    }

    override fun render(): String {
        return "$mnemonic ${value.render(8)}"
    }
}
