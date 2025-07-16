package de.mr_pine.c0ne.ir.optimize

import de.mr_pine.c0ne.ir.GraphConstructor
import de.mr_pine.c0ne.ir.node.Node

/** An interface that allows replacing a node with a more optimal one. */
interface Optimizer {
    context(constructor: GraphConstructor)
    fun transform(node: Node): Node
}
