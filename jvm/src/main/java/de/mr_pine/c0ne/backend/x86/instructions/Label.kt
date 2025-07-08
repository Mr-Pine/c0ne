package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc
import de.mr_pine.c0ne.ir.node.Block

class Label(val block: Block) : Instruction("block_${processLabel(block.toString())}_${block.hashCode()}") {
    context(alloc: X86RegAlloc)
    override fun concretize() = this
    override fun render(size: Int): String {
        return "$mnemonic:"
    }

    companion object {
        fun processLabel(blockName: String) = blockName.replace(" ", "_")
            .replace('-', '_')
    }
}
