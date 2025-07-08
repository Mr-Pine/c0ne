package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc
import de.mr_pine.c0ne.backend.x86.X86CodeGenerator
import de.mr_pine.c0ne.backend.x86.X86CodeGenerator.AbstractCodegen.Companion.callerSaved
import de.mr_pine.c0ne.backend.x86.instructions.Argument.RegMem.Register.RealRegister
import de.mr_pine.c0ne.parser.symbol.Name

class Call(val target: Name, val returnTarget: Argument, val callArguments: List<Argument>) : Instruction("CALL") {
    context(alloc: X86RegAlloc) override fun concretize(): Instruction {
        return Call(target, returnTarget.concretize(), callArguments.map { it.concretize() })
    }

    override fun render(size: Int): String {
        require(size == 4) { "Size must be 4 for CALL" }
        val toSave = callerSaved.filter { it != returnTarget }
        val callerSave = toSave.joinToString("\n") {
            Push(it).render()
        }
        val callerRestore = toSave.reversed().joinToString("\n") {
            Pop(it).render()
        }
        val valueStore = Mov(
            returnTarget, RealRegister.RAX
        ).render()

        val argumentsWithLocations =
            callArguments.zip(X86CodeGenerator.AbstractCodegen.arguments.take(callArguments.size).toList())
        val registerArguments = argumentsWithLocations.filter { it.second is Argument.RegMem.Register }
        val memoryArguments = argumentsWithLocations.filter { it.second is Argument.RegMem.StackOverflowSlot }
        val argumentMoves = memoryArguments.reversed().joinToString("\n") { (value, _) ->
            Push(value).render()
        } + "\n" + registerArguments.joinToString("\n") { (value, _) ->
            Push(value).render()
        } + "\n" + registerArguments.reversed().joinToString("\n") { (_, target) ->
            Pop(target).render()
        }
        val stackArgReset = if (memoryArguments.isNotEmpty()) {
            Add(
                RealRegister.RSP,
                Argument.Immediate(8 * memoryArguments.size)
            ).render(8)
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
