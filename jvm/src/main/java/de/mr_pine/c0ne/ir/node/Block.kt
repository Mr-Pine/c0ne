package de.mr_pine.c0ne.ir.node

import de.mr_pine.c0ne.ir.IrGraph

class Block(graph: IrGraph, val label: String) : Node(graph) {
    override fun toString() = "Block $label"
}
