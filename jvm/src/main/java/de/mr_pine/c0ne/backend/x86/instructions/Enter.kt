package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc
import de.mr_pine.c0ne.ir.node.StartNode

class Enter(val node: StartNode, val overflowCount: Int? = null) : Instruction("ENTER") {
    context(alloc: NextGenSimpleX86RegAlloc)
    override fun concretize() = Enter(node, alloc.overflowCount)
    override fun render(): String {
        return """
            .global ${node.graph.name()}
            ${node.graph.name()}:
            $mnemonic ${overflowCount!! * 4}, 0
        """.trimIndent()
    }
}
