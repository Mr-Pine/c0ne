package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.visitor.SSAVisitor
import de.mr_pine.c0ne.parser.symbol.Name

class ProjNode(block: Block, input: Node, private val projectionInfo: ProjectionInfo) : Node(block, input) {
    override fun info(): String? {
        return this.projectionInfo.toString()
    }

    fun projectionInfo(): ProjectionInfo {
        return projectionInfo
    }

    interface ProjectionInfo

    enum class SimpleProjectionInfo : ProjectionInfo {
        RESULT, SIDE_EFFECT, IF_TRUE, IF_FALSE
    }

    data class NamedParameterProjectionInfo(val name: Name, val index: Int): ProjectionInfo

    companion object {
        const val IN: Int = 0
    }

    override fun <R> accept(visitor: SSAVisitor<R>): R {
        return visitor.visit(this)
    }
}
