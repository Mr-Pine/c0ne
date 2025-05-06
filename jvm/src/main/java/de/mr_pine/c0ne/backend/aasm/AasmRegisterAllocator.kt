package de.mr_pine.c0ne.backend.aasm

import de.mr_pine.c0ne.backend.RegisterAllocator
import de.mr_pine.c0ne.backend.Register
import edu.kit.kastel.vads.compiler.ir.IrGraph
import edu.kit.kastel.vads.compiler.ir.node.Block
import edu.kit.kastel.vads.compiler.ir.node.Node
import edu.kit.kastel.vads.compiler.ir.node.ProjNode
import edu.kit.kastel.vads.compiler.ir.node.ReturnNode
import edu.kit.kastel.vads.compiler.ir.node.StartNode

class AasmRegisterAllocator : RegisterAllocator {
    var counter = 0
    var registerMap = mutableMapOf<Node, Register>()

    override fun allocateRegisters(graph: IrGraph): RegisterAllocator.RegisterAllocation {
        val visited = mutableSetOf<Node>()
        visited.add(graph.endBlock())
        scan(graph.endBlock(), visited)
        return AasmRegisterAllocation(registerMap)
    }

    fun scan(node: Node, visited: MutableSet<Node>) {
        for (predecessor in node.predecessors()) {
            if (predecessor !in visited) {
                visited.add(predecessor)
                scan(predecessor, visited)
            }
        }
        if (node.needsRegister) {
            registerMap[node] = VirtualRegister(counter++)
        }
    }

    data class AasmRegisterAllocation(private val registerMap: Map<Node, Register>) :
        RegisterAllocator.RegisterAllocation {
        override fun get(node: Node) {
            registerMap[node]
        }

    }
}

private val Node.needsRegister: Boolean
    get() = !(this is ProjNode || this is StartNode || this is Block || this is ReturnNode)