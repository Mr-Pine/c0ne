package de.mr_pine.c0ne.backend.x86

import de.mr_pine.c0ne.backend.AllocationInterferenceGraph
import de.mr_pine.c0ne.backend.RegisterAllocator
import de.mr_pine.c0ne.backend.needsRegister
import de.mr_pine.c0ne.ir.IrGraph
import de.mr_pine.c0ne.ir.node.Node

class X86ColoringRegisterAllocator :
    RegisterAllocator<X86Register, X86ColoringRegisterAllocator.X86ColoringRegisterAllocation> {

    private val registerMap = mutableMapOf<Node, X86Register>()
    private val registerSequence = (X86Register.RealRegister.entries - listOf(
        X86Register.RealRegister.RAX,
        X86Register.RealRegister.RDX,
        X86Register.RealRegister.RBP,
        X86Register.RealRegister.RSP,
        X86Register.RealRegister.R15
    )).asSequence() + generateSequence(X86Register.OverflowSlot(0)) { X86Register.OverflowSlot(it.index + 1) }

    fun allocateFromSimplicialOrdering(ordering: List<Node>, interferenceGraph: AllocationInterferenceGraph) {
        val relevant = ordering.filter(Node::needsRegister)
        val soFar = mutableSetOf<Node>()
        for (node in relevant) {
            val availableRegisters = registerSequence.filter { reg ->
                reg !in interferenceGraph.neighbourhood(node).intersect(soFar).map { registerMap[it] }
            }
            registerMap[node] = availableRegisters.first()
            soFar.add(node)
        }
    }

    override fun allocateRegisters(graph: IrGraph): X86ColoringRegisterAllocation {
        TODO()
        /*val interferenceGraph = AllocationInterferenceGraph(graph)
        val simplicialOrdering = interferenceGraph.buildSimplicialOrdering()
        allocateFromSimplicialOrdering(simplicialOrdering, interferenceGraph)
        val usedOverflowSlots = registerMap.values.mapNotNull { it as? X86Register.OverflowSlot }.maxOfOrNull(
            X86Register.OverflowSlot::index
        ) ?: 0
        println("Used overflow slots: $usedOverflowSlots")
        return X86ColoringRegisterAllocation(registerMap, usedOverflowSlots)*/
    }


    data class X86ColoringRegisterAllocation(
        private val allocations: Map<Node, X86Register>, override val overflowCount: Int
    ) : X86RegisterAllocation {
        override fun getOrNull(node: Node) = allocations[node]

    }
}