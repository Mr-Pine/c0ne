package de.mr_pine.c0ne.backend.x86

import de.mr_pine.c0ne.backend.RegisterAllocator
import edu.kit.kastel.vads.compiler.ir.IrGraph
import edu.kit.kastel.vads.compiler.ir.node.Block
import edu.kit.kastel.vads.compiler.ir.node.Node
import edu.kit.kastel.vads.compiler.ir.node.ProjNode
import edu.kit.kastel.vads.compiler.ir.node.ReturnNode
import edu.kit.kastel.vads.compiler.ir.node.StartNode

class X86StraightLineRegisterAllocator :
    RegisterAllocator<X86Register, X86StraightLineRegisterAllocator.X86StraightLineRegisterAllocation> {
    private val registerMap: MutableMap<Node, X86Register> = mutableMapOf()
    private val remainingRealRegisters: ArrayDeque<X86Register.RealRegister> =
        ArrayDeque(X86Register.RealRegister.entries.filter(X86Register.RealRegister::availableForAllocation))
    var overflowCount: Int = 0
        private set

    override fun allocateRegisters(graph: IrGraph): X86StraightLineRegisterAllocation {
        val visited = mutableSetOf<Node>()
        visited.add(graph.endBlock())
        scan(graph.endBlock(), visited)
        return X86StraightLineRegisterAllocation(registerMap, overflowCount)
    }

    fun scan(node: Node, visited: MutableSet<Node>) {
        for (predecessor in node.predecessors()) {
            if (predecessor !in visited) {
                visited.add(predecessor)
                scan(predecessor, visited)
            }
        }
        if (node.needsRegister) {
            val real = remainingRealRegisters.removeFirstOrNull();
            if (real != null) {
                registerMap[node] = real
            } else {
                registerMap[node] = X86Register.OverflowSlot(overflowCount++)
            }
        }
    }

    data class X86StraightLineRegisterAllocation(
        private val registerMap: Map<Node, X86Register>, override val overflowCount: Int
    ) : X86RegisterAllocation {
        override fun get(node: Node) = registerMap[node]!!
    }
}

private val Node.needsRegister: Boolean
    get() = !(this is ProjNode || this is StartNode || this is Block || this is ReturnNode)