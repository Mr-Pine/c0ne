package de.mr_pine.c0ne.ir.optimize

import edu.kit.kastel.vads.compiler.ir.node.Node

/** An interface that allows replacing a node with a more optimal one. */
interface Optimizer {
    fun transform(node: Node): Node
}
