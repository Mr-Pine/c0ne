package de.mr_pine.c0ne.ir.util

import de.mr_pine.c0ne.ir.node.Node
import de.mr_pine.c0ne.ir.node.ProjNode

object NodeSupport {
    fun predecessorSkipSimpleProj(node: Node, predIdx: Int): Node {
        val pred = node.predecessor(predIdx)
        if (pred is ProjNode && pred.projectionInfo() is ProjNode.SimpleProjectionInfo) {
            return pred.predecessor(ProjNode.IN)
        }
        return pred
    }
}
