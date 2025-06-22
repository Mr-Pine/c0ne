package de.mr_pine.c0ne.ir.optimize

import de.mr_pine.c0ne.ir.node.Node

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