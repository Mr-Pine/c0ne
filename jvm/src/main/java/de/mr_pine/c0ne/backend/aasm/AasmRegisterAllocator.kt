package de.mr_pine.c0ne.backend.aasm

import de.mr_pine.c0ne.backend.RegisterAllocator
import de.mr_pine.c0ne.analysis.nodesInControlFlowOrder
import de.mr_pine.c0ne.ir.IrGraph
import edu.kit.kastel.vads.compiler.ir.node.*

class AasmRegisterAllocator : RegisterAllocator<VirtualRegister, AasmRegisterAllocator.AasmRegisterAllocation> {
    private var counter = 0
    private var registerMap = mutableMapOf<Node, VirtualRegister>()

    override fun allocateRegisters(graph: IrGraph): AasmRegisterAllocation {
        val nodes = graph.nodesInControlFlowOrder()
        for (node in nodes) {
            if (node.needsRegister) {
                registerMap[node] = VirtualRegister(counter++)
            }
        }
        return AasmRegisterAllocation(registerMap)
    }

    data class AasmRegisterAllocation(private val registerMap: Map<Node, VirtualRegister>) :
        RegisterAllocator.RegisterAllocation<VirtualRegister> {
        override fun getOrNull(node: Node) = registerMap[node]

    }
}

private val Node.needsRegister: Boolean
    get() = !(this is ProjNode || this is StartNode || this is Block || this is ReturnNode)