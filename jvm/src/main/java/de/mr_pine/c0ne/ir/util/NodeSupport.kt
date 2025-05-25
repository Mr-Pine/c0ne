package de.mr_pine.c0ne.ir.util

import de.mr_pine.c0ne.ir.node.Node
import de.mr_pine.c0ne.ir.node.ProjNode

object NodeSupport {
    fun predecessorSkipProj(node: Node, predIdx: Int): Node {
        val pred = node.predecessor(predIdx)
        if (pred is ProjNode) {
            return pred.predecessor(ProjNode.IN)
        }
        return pred
    }
}
