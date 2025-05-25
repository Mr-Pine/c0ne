package de.mr_pine.c0ne.ir.node

class Phi(block: Block) : Node(block) {
    fun appendOperand(node: Node) {
        addPredecessor(node)
    }
}
