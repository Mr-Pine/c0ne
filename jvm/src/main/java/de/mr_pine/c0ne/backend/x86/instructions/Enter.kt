package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc
import de.mr_pine.c0ne.backend.x86.X86CodeGenerator
import de.mr_pine.c0ne.ir.node.StartNode

class Enter(val node: StartNode, val parameters: List<Argument?>, val overflowCount: Int? = null) :
    Instruction("ENTER") {
    context(alloc: X86RegAlloc)
    override fun concretize() = Enter(node, parameters.map { it?.concretize() }, alloc.overflowCount)

    override fun render(size: Int): String {
        require(size == 4) { "Size must be 4 for ENTER" }
        val calleeSavePush = X86CodeGenerator.AbstractCodegen.calleeSaved.joinToString("\n") {
            Push(it).render()
        }
        val parameterMove =
            X86CodeGenerator.AbstractCodegen.arguments.take(parameters.size)
                .filterIndexed { index, _ -> parameters[index] != null }.toList().reversed().joinToString("\n") {
                Push(it).render()
            } + "\n" + parameters.filterNotNull().joinToString("\n") { value ->
                Pop(value).render()
            }
        return """
            .global ${node.graph.name()}
            ${node.graph.name()}:
            $mnemonic ${overflowCount!! * 8}, 0
            $calleeSavePush
            $parameterMove
        """.trimIndent()
    }
}
