package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc
import de.mr_pine.c0ne.backend.x86.NextGenX86CodeGenerator
import de.mr_pine.c0ne.backend.x86.NextGenX86CodeGenerator.AbstractCodegen.Companion.callerSaved
import de.mr_pine.c0ne.backend.x86.X86Register
import de.mr_pine.c0ne.parser.symbol.Name

class Call(val target: Name, val returnTarget: Argument, val callArguments: List<Argument>) : Instruction("CALL") {
    context(alloc: NextGenSimpleX86RegAlloc)
    override fun concretize(): Instruction {
        return Call(target, returnTarget.concretize(), callArguments.map { it.concretize() })
    }

    override fun render(): String {
        val toSave = callerSaved.filter { Argument.RegMem.Register.RealRegister(it) != returnTarget }
        val callerSave = toSave.joinToString("\n") {
            Push(Argument.RegMem.Register.RealRegister(it)).render()
        }
        val callerRestore = toSave.reversed().joinToString("\n") {
            Pop(Argument.RegMem.Register.RealRegister(it)).render()
        }
        val valueStore = Mov(
            returnTarget, Argument.RegMem.Register.RealRegister(X86Register.RealRegister.RAX)
        ).render()
        val argumentMoves =
            callArguments.zip(NextGenX86CodeGenerator.AbstractCodegen.arguments)
                .joinToString("\n") { (source, target) ->
                    Mov(Argument.RegMem.Register.RealRegister(target), source).render()
                }
        return """
            $callerSave
            $argumentMoves
            $mnemonic ${target.asString()}
            $valueStore
            $callerRestore
            
        """.trimIndent()
    }
}
