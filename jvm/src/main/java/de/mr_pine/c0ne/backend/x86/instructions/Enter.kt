package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc
import de.mr_pine.c0ne.backend.x86.NextGenX86CodeGenerator
import de.mr_pine.c0ne.ir.node.StartNode

class Enter(val node: StartNode, val parameters: List<Argument>, val overflowCount: Int? = null) :
    Instruction("ENTER") {
    context(alloc: NextGenSimpleX86RegAlloc)
    override fun concretize() = Enter(node, parameters.map { it.concretize() }, alloc.overflowCount)

    override fun render(): String {
        val calleeSavePush = NextGenX86CodeGenerator.AbstractCodegen.calleeSaved.joinToString("\n") {
            Push(Argument.RegMem.Register.RealRegister(it)).render()
        }
        val parameterMove = parameters.zip(NextGenX86CodeGenerator.AbstractCodegen.arguments).joinToString("\n") { (target, source) ->
            Mov(target, Argument.RegMem.Register.RealRegister(source)).render()
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
