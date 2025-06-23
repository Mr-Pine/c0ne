package de.mr_pine.c0ne.backend.x86

import de.mr_pine.c0ne.backend.AllocationInterferenceGraph
import de.mr_pine.c0ne.backend.Schedule
import de.mr_pine.c0ne.backend.needsRegister
import de.mr_pine.c0ne.backend.x86.instructions.Argument
import de.mr_pine.c0ne.ir.node.Block
import de.mr_pine.c0ne.ir.node.ConstBoolNode
import de.mr_pine.c0ne.ir.node.ConstIntNode
import de.mr_pine.c0ne.ir.node.Node
import de.mr_pine.c0ne.ir.node.ProjNode
import kotlin.collections.set

class NextGenSimpleX86RegAlloc(private val startBlock: Block, private val schedule: Schedule) {
    private var allocatable = (X86Register.RealRegister.entries - listOf(
        X86Register.RealRegister.RAX,
        X86Register.RealRegister.RDX,
        X86Register.RealRegister.RCX,
        X86Register.RealRegister.RBP,
        X86Register.RealRegister.RSP,
        X86Register.RealRegister.R15
    )).map { Argument.RegMem.Register.RealRegister(it) }
        .asSequence() + generateSequence(Argument.RegMem.StackOverflowSlot(0)) { Argument.RegMem.StackOverflowSlot(it.index + 1) }

    private val allocation = allocateRegisters()

    fun allocateRegisters(): Map<Node, Argument.RegMem> {
        val interferenceGraph = AllocationInterferenceGraph(schedule, startBlock)
        val simplicialOrdering = interferenceGraph.buildSimplicialOrdering()
        return allocateFromSimplicialOrdering(simplicialOrdering, interferenceGraph)
    }

    fun allocateFromSimplicialOrdering(ordering: List<Node>, interferenceGraph: AllocationInterferenceGraph): Map<Node, Argument.RegMem> = buildMap {
        val relevant = ordering.filter(Node::needsRegister)
        val soFar = mutableSetOf<Node>()
        for (node in relevant) {
            val availableRegisters = allocatable.filter { reg ->
                reg !in interferenceGraph.neighbourhood(node).intersect(soFar).map { this[it] }
            }
            set(node, availableRegisters.first())
            soFar.add(node)
        }
    }

    val overflowCount
        get() = allocation.values.mapNotNull { it as? Argument.RegMem.StackOverflowSlot }.maxOfOrNull { it.index }?.let { it + 1 } ?: 0

    fun concretize(argument: Argument.NodeValue): Argument {
        return when (argument.node) {
            is ConstIntNode -> Argument.Immediate(argument.node.value)
            is ConstBoolNode -> Argument.Immediate(if (argument.node.value) 1 else 0)
            else -> allocation[argument.nodeValue.node] ?: error("No register allocated for $argument")
        }
    }

    fun concretize(argument: Argument.RegMem.Register.RegisterFor): Argument.RegMem.Register.RealRegister {
        val concreteArg = argument.arg.concretize()
        return concreteArg as? Argument.RegMem.Register.RealRegister
            ?: Argument.RegMem.Register.RealRegister(X86Register.RealRegister.R15)
    }

    fun concretize(argument: Argument.RegMem.RegMemFor): Argument {
        val concreteArg = argument.arg.concretize()
        return concreteArg as? Argument.RegMem
            ?: Argument.RegMem.Register.RealRegister(X86Register.RealRegister.R15)
    }

    fun concretize(argument: Argument.RegMem.StackOverflowSlot) = argument
    fun concretize(argument: Argument.RegMem.Register.RealRegister) = argument
    fun concretize(argument: Argument.Immediate) = argument
    fun concretize(argument: Argument.RegMem.Register.EcxOf) = Argument.RegMem.Register.RealRegister(X86Register.RealRegister.RCX)

}