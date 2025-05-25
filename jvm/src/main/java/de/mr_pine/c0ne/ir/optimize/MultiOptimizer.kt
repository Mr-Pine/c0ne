package de.mr_pine.c0ne.ir.optimize

import edu.kit.kastel.vads.compiler.ir.node.Node

class MultiOptimizer(val optimizers: List<Optimizer>) : Optimizer {
    constructor(vararg optimizers: Optimizer) : this(optimizers.toList())

    override fun transform(node: Node): Node {
        var currentNode = node
        for (optimizer in optimizers) {
            currentNode = optimizer.transform(currentNode)
        }
        return currentNode
    }
}