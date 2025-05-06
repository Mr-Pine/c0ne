package de.mr_pine.c0ne.backend

import edu.kit.kastel.vads.compiler.ir.IrGraph
import edu.kit.kastel.vads.compiler.ir.node.Node

interface RegisterAllocator<A: RegisterAllocator.RegisterAllocation> {
    fun allocateRegisters(graph: IrGraph): A

    interface RegisterAllocation {
        operator fun get(node: Node): Register
    }
}