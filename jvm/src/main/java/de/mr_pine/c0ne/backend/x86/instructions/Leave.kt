package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc
import de.mr_pine.c0ne.backend.x86.X86CodeGenerator

class Leave : Instruction("LEAVE") {
    context(alloc: X86RegAlloc)
    override fun concretize() = this

    override fun render(size: Int): String {
        require(size == 4) { "Size must be 4 for LEAVE" }
        val calleeSavePop = X86CodeGenerator.AbstractCodegen.calleeSaved.reversed().joinToString("\n") {
            Pop(it).render()
        }
        return """
            $calleeSavePop
            $mnemonic
        """.trimIndent()
    }
}
