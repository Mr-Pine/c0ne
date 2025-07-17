package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

class Cmp(val target: Argument, val source: Argument, val size: Int = 4) : Instruction("CMP", target, source) {
    context(alloc: X86RegAlloc)
    override fun concretize(): Instruction {
        return Cmp(target.concretize(), source.concretize(), size)
    }

    override fun render(size: Int): String {
        require(size == this.size || size == 4)
        return "$mnemonic ${arguments.joinToString(", ") {it.render(this.size)}}"
    }
}
