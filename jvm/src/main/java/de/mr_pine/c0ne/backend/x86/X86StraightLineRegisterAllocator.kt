package de.mr_pine.c0ne.backend.x86

import de.mr_pine.c0ne.backend.RegisterAllocator
import de.mr_pine.c0ne.analysis.nodesInControlFlowOrder
import de.mr_pine.c0ne.backend.needsRegister
import edu.kit.kastel.vads.compiler.ir.IrGraph
import edu.kit.kastel.vads.compiler.ir.node.*

class X86StraightLineRegisterAllocator :
    RegisterAllocator<X86Register, X86StraightLineRegisterAllocator.X86StraightLineRegisterAllocation> {
    private val registerMap: MutableMap<Node, X86Register> = mutableMapOf()
    private val remainingRealRegisters: ArrayDeque<X86Register.RealRegister> =
        ArrayDeque(X86Register.RealRegister.entries - listOf(X86Register.RealRegister.EAX, X86Register.RealRegister.EDX, X86Register.RealRegister.RBP, X86Register.RealRegister.RSP, X86Register.RealRegister.R15D))
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
        println("Used overflow slots: $overflowCount")
        return X86StraightLineRegisterAllocation(registerMap, overflowCount)
    }

    data class X86StraightLineRegisterAllocation(
        private val registerMap: Map<Node, X86Register>, override val overflowCount: Int
    ) : X86RegisterAllocation {
        override fun getOrNull(node: Node) = registerMap[node]
    }
}