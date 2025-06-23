package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.node.ProjNode.SimpleProjectionInfo
import de.mr_pine.c0ne.ir.util.NodeSupport
import de.mr_pine.c0ne.ir.visitor.SSAVisitor

class Phi(block: Block) : Node(block) {
    fun appendOperand(node: Node) {
        addPredecessor(node)
    }

    operator fun get(index: Int) = NodeSupport.predecessorSkipSimpleProj(this, index)

    val isSideEffectPhi
        get() = checkSideeffect(mutableSetOf())

    private val Node.isDirectSideeffect
        get() = this is ProjNode && this.projectionInfo() === SimpleProjectionInfo.SIDE_EFFECT

    private fun Phi.checkSideeffect(visited: MutableSet<Phi>): Boolean {
        visited.add(this)
        return predecessors().any { it.isDirectSideeffect } || predecessors().filter { it !in visited }
            .mapNotNull { it as? Phi }.any { it.checkSideeffect(visited) }
    }

    override fun <R> accept(visitor: SSAVisitor<R>): R {
        return visitor.visit(this)
    }
}
