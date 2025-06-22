package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc

class Cmp(val target: Argument.RegMem, val source: Argument, val size: Int = 4) : Instruction("CMP", target, source) {
    context(alloc: NextGenSimpleX86RegAlloc)
    override fun concretize(): Instruction {
        return Cmp(target.concretize() as Argument.RegMem, source.concretize(), size)
    }

    override fun render(): String {
        return "$mnemonic ${arguments.joinToString(", ") {it.render(size)}}"
    }
}
