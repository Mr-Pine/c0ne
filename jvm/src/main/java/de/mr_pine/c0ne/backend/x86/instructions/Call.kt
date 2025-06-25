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

    override fun render(size: Int): String {
        require(size == 4) { "Size must be 4 for CALL" }
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

        val argumentsWithLocations =
            callArguments.zip(NextGenX86CodeGenerator.AbstractCodegen.arguments.take(callArguments.size).toList())
        val registerArguments = argumentsWithLocations.filter { it.second is Argument.RegMem.Register }
        val memoryArguments = argumentsWithLocations.filter { it.second is Argument.RegMem.StackOverflowSlot }
        val argumentMoves =
            registerArguments.joinToString("\n") { (value, _) ->
                Push(value).render()
            } + "\n" + registerArguments.reversed().joinToString("\n") { (_, target) ->
                Pop(target).render()
            } + "\n" + memoryArguments.reversed().joinToString("\n") { (value, _) ->
                Push(value).render()
            }
        val stackArgReset = if (memoryArguments.isNotEmpty()) {
            Add(
                Argument.RegMem.Register.RealRegister(X86Register.RealRegister.RSP),
                Argument.Immediate(8 * memoryArguments.size)
            )
                .render(8)
        } else ""

        return """
            $callerSave
            $argumentMoves
            $mnemonic ${target.asString()}
            $stackArgReset
            $valueStore
            $callerRestore
            
        """.trimIndent()
    }
}
