package de.mr_pine.c0ne.backend.x86

import de.mr_pine.c0ne.backend.RegisterAllocator
import de.mr_pine.c0ne.ir.left
import de.mr_pine.c0ne.ir.result
import de.mr_pine.c0ne.ir.right
import edu.kit.kastel.vads.compiler.ir.IrGraph
import edu.kit.kastel.vads.compiler.ir.node.AddNode
import edu.kit.kastel.vads.compiler.ir.node.Block
import edu.kit.kastel.vads.compiler.ir.node.ConstIntNode
import edu.kit.kastel.vads.compiler.ir.node.DivNode
import edu.kit.kastel.vads.compiler.ir.node.ModNode
import edu.kit.kastel.vads.compiler.ir.node.MulNode
import edu.kit.kastel.vads.compiler.ir.node.Node
import edu.kit.kastel.vads.compiler.ir.node.ProjNode
import edu.kit.kastel.vads.compiler.ir.node.ReturnNode
import edu.kit.kastel.vads.compiler.ir.node.StartNode
import edu.kit.kastel.vads.compiler.ir.node.SubNode

class X86ColoringRegisterAllocator :
    RegisterAllocator<X86Register, X86ColoringRegisterAllocator.X86ColoringRegisterAllocation> {
    private data class Conflict(private val memberA: Node, private val memberB: Node) {
        fun involves(node: Node) = memberA == node || memberB == node
        fun getConflicting(forNode: Node): Node? {
            if (memberA == forNode) return memberB
            if (memberB == forNode) return memberA
            return null
        }
    }

    private val conflicts = mutableSetOf<Conflict>()
    private val liveNodes = mutableSetOf<Node>()
    fun addLiveConflicts(node: Node) {
        for (live in liveNodes.filter { it != node }) {
            conflicts.add(Conflict(node, live))
        }
    }

    fun killNode(node: Node) {
        liveNodes.remove(node)
    }

    private val registerMap = mutableMapOf<Node, X86Register>()
    private val registerSequence =
        X86Register.RealRegister.entries.filter(X86Register.RealRegister::availableForAllocation)
            .asSequence() + generateSequence(X86Register.OverflowSlot(0)) { X86Register.OverflowSlot(it.index + 1) }


    fun scanConflicts(node: Node, visited: MutableSet<Node>) {
        when (node) {
            is AddNode, is MulNode, is SubNode -> {
                addLiveConflicts(node)
                liveNodes.add(node.left)
                liveNodes.add(node.right)
                killNode(node)
            }

            is DivNode, is ModNode -> {
                addLiveConflicts(node)
                liveNodes.add(node.left)
                liveNodes.add(node.right)
                killNode(node)
            }

            is ConstIntNode -> {
                addLiveConflicts(node)
                killNode(node)
            }

            is ReturnNode -> liveNodes.add(node.result)
            else -> {}
        }
        for (predecessor in node.predecessors()) {
            if (predecessor !in visited) {
                visited.add(predecessor)
                scanConflicts(predecessor, visited)
            }
        }
    }

    override fun allocateRegisters(graph: IrGraph): X86ColoringRegisterAllocation {
        val conflictVisited = mutableSetOf<Node>(graph.endBlock())
        scanConflicts(graph.endBlock(), conflictVisited)
        val visited = mutableSetOf<Node>(graph.endBlock())
        scan(graph.endBlock(), visited)
        return X86ColoringRegisterAllocation(
            registerMap, registerMap.values.mapNotNull { it as? X86Register.OverflowSlot }.maxOfOrNull(
                X86Register.OverflowSlot::index
            ) ?: 0
        )
    }

    fun scan(node: Node, visited: MutableSet<Node>) {
        for (predecessor in node.predecessors()) {
            if (predecessor !in visited) {
                visited.add(predecessor)
                scan(predecessor, visited)
            }
        }
        if (node.needsRegister) {
            val availableRegisters = registerSequence.filter { reg ->
                reg !in conflicts.mapNotNull {
                    it.getConflicting(node)?.let { registerMap[it] }
                }
            }
            registerMap[node] = availableRegisters.first()
        }
    }

    data class X86ColoringRegisterAllocation(
        private val allocations: Map<Node, X86Register>,
        override val overflowCount: Int
    ) : X86RegisterAllocation {
        override fun get(node: Node) = allocations[node]!!

    }
}

private val Node.needsRegister: Boolean
    get() = !(this is ProjNode || this is StartNode || this is Block || this is ReturnNode)