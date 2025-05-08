package de.mr_pine.c0ne.backend.x86

import de.mr_pine.c0ne.backend.RegisterAllocator
import de.mr_pine.c0ne.backend.nodesInControlFlowOrder
import edu.kit.kastel.vads.compiler.ir.IrGraph
import edu.kit.kastel.vads.compiler.ir.node.*

class X86StraightLineRegisterAllocator :
    RegisterAllocator<X86Register, X86StraightLineRegisterAllocator.X86StraightLineRegisterAllocation> {
    private val registerMap: MutableMap<Node, X86Register> = mutableMapOf()
    private val remainingRealRegisters: ArrayDeque<X86Register.RealRegister> =
        ArrayDeque(X86Register.RealRegister.entries.filter(X86Register.RealRegister::availableForAllocation))
    var overflowCount: Int = 0
        private set

    override fun allocateRegisters(graph: IrGraph): X86StraightLineRegisterAllocation {
        val nodes = graph.nodesInControlFlowOrder()
        for (node in nodes) {
            if (node.needsRegister) {
                val real = remainingRealRegisters.removeFirstOrNull()
                if (real != null) {
                    registerMap[node] = real
                } else {
                    registerMap[node] = X86Register.OverflowSlot(overflowCount++)
                }
            }
        }
        return X86StraightLineRegisterAllocation(registerMap, overflowCount)
    }

    data class X86StraightLineRegisterAllocation(
        private val registerMap: Map<Node, X86Register>, override val overflowCount: Int
    ) : X86RegisterAllocation {
        override fun get(node: Node) = registerMap[node]!!
    }
}

private val Node.needsRegister: Boolean
    get() = !(this is ProjNode || this is StartNode || this is Block || this is ReturnNode)