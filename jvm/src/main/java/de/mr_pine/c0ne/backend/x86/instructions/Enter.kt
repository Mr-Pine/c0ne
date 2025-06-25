package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc
import de.mr_pine.c0ne.backend.x86.NextGenX86CodeGenerator
import de.mr_pine.c0ne.ir.node.StartNode

class Enter(val node: StartNode, val parameters: List<Argument?>, val overflowCount: Int? = null) :
    Instruction("ENTER") {
    context(alloc: NextGenSimpleX86RegAlloc)
    override fun concretize() = Enter(node, parameters.map { it?.concretize() }, alloc.overflowCount)

    override fun render(): String {
        val calleeSavePush = NextGenX86CodeGenerator.AbstractCodegen.calleeSaved.joinToString("\n") {
            Push(Argument.RegMem.Register.RealRegister(it)).render()
        }
        val parameterMove =
            NextGenX86CodeGenerator.AbstractCodegen.arguments.take(parameters.size).filterIndexed { index, _ -> parameters[index] != null }.reversed().joinToString("\n") {
                Push(Argument.RegMem.Register.RealRegister(it)).render()
            } + "\n" + parameters.filterNotNull().joinToString("\n") { value ->
                Pop(value).render()
            }
        return """
            .global ${node.graph.name()}
            ${node.graph.name()}:
            $mnemonic ${overflowCount!! * 4}, 0
            $calleeSavePush
            $parameterMove
        """.trimIndent()
    }
}
