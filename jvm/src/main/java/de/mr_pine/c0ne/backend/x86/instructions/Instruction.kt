package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

sealed class Instruction(val mnemonic: String, vararg val arguments: Argument) {
    override fun toString() = "$mnemonic ${arguments.joinToString(", ")}"

    val sources
        get() = arguments

    context(alloc: X86RegAlloc)
    abstract fun concretize(): Instruction
    open fun render(size: Int = 4): String {
        return "$mnemonic ${arguments.joinToString(", ") {it.render(size)}}"
    }
}

