package de.mr_pine.c0ne.backend

import de.mr_pine.c0ne.ir.IrGraph
import edu.kit.kastel.vads.compiler.ir.node.Node

interface RegisterAllocator<R: Register, A: RegisterAllocator.RegisterAllocation<R>> {
    fun allocateRegisters(graph: IrGraph): A

    interface RegisterAllocation<R: Register> {
        operator fun get(node: Node) = getOrNull(node)!!

        fun getOrNull(node: Node): R?
    }
}