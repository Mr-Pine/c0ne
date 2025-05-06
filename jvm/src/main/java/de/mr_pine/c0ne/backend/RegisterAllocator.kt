package de.mr_pine.c0ne.backend

import edu.kit.kastel.vads.compiler.ir.IrGraph
import edu.kit.kastel.vads.compiler.ir.node.Node

interface RegisterAllocator {
    fun allocateRegisters(graph: IrGraph): RegisterAllocation

    interface RegisterAllocation {
        operator fun get(node: Node)
    }
}