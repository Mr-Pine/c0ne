package de.mr_pine.c0ne.ir.optimize

import de.mr_pine.c0ne.ir.GraphConstructor
import de.mr_pine.c0ne.ir.node.CallNode
import de.mr_pine.c0ne.ir.node.ConstIntNode
import de.mr_pine.c0ne.ir.node.MemoryReadNode
import de.mr_pine.c0ne.ir.node.Node
import de.mr_pine.c0ne.parser.symbol.IdentName

class ArraySizeRead : Optimizer {
    context(constructor: GraphConstructor)
    override fun transform(node: Node): Node {
        val isMemoryReadFromAlloc =
            node is MemoryReadNode && node.base is CallNode && (node.base as CallNode).target == IdentName("alloc")
        val isArrayReadFromAlloc =
            isMemoryReadFromAlloc && node.offset is ConstIntNode && (node.offset as ConstIntNode).value == 0

        if (isArrayReadFromAlloc) {
            return node.base.predecessor(1)
        }
        return node
    }
}