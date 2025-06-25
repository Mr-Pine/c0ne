package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc
import de.mr_pine.c0ne.backend.x86.NextGenX86CodeGenerator

class Leave : Instruction("LEAVE") {
    context(alloc: NextGenSimpleX86RegAlloc)
    override fun concretize() = this

    override fun render(size: Int): String {
        require(size == 4) { "Size must be 4 for LEAVE" }
        val calleeSavePop = NextGenX86CodeGenerator.AbstractCodegen.calleeSaved.reversed().joinToString("\n") {
            Pop(Argument.RegMem.Register.RealRegister(it)).render()
        }
        return """
            $calleeSavePop
            $mnemonic
        """.trimIndent()
    }
}
