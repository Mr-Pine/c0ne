package de.mr_pine.c0ne.ir.node

class ProjNode(block: Block, input: Node, private val projectionInfo: ProjectionInfo) : Node(block, input) {
    override fun info(): String? {
        return this.projectionInfo.toString()
    }

    fun projectionInfo(): ProjectionInfo {
        return projectionInfo
    }

    interface ProjectionInfo

    enum class SimpleProjectionInfo : ProjectionInfo {
        RESULT, SIDE_EFFECT
    }

    companion object {
        const val IN: Int = 0
    }
}
