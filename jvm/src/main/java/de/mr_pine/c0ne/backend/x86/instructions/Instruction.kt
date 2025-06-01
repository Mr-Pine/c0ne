package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc

sealed class Instruction(val mnemonic: String, vararg val arguments: Argument) {
    override fun toString() = "$mnemonic ${arguments.joinToString(", ")}"

    val sources
        get() = arguments

    context(alloc: NextGenSimpleX86RegAlloc)
    abstract fun concretize(): Instruction
}

