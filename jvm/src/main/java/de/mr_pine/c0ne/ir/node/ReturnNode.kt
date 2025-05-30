package de.mr_pine.c0ne.ir.node

class ReturnNode(block: Block, sideEffect: Node, result: Node) : Node(block, sideEffect, result), ControlNode {
    companion object {
        const val SIDE_EFFECT: Int = 0
        const val RESULT: Int = 1
    }
}
