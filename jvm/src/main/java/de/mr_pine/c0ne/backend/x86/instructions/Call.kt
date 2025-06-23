package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc
import de.mr_pine.c0ne.backend.x86.NextGenX86CodeGenerator
import de.mr_pine.c0ne.parser.symbol.Name

class Call(val target: Name, val callArguments: List<Argument>) : Instruction("CALL") {
    context(alloc: NextGenSimpleX86RegAlloc)
    override fun concretize(): Instruction {
        return Call(target, callArguments.map { it.concretize() })
    }

    override fun render(): String {
        val argumentMoves =
            callArguments.zip(NextGenX86CodeGenerator.AbstractCodegen.arguments).joinToString("\n") { (source, target) ->
                Mov(Argument.RegMem.Register.RealRegister(target), source).render()
            }
        return """
            $argumentMoves
            $mnemonic ${target.asString()}
        """.trimIndent()
    }
}
