package de.mr_pine.c0ne.ir.node

class ReturnNode(block: Block, sideEffect: Node, result: Node) : ExitNode(block, sideEffect, result) {
    companion object {
        const val SIDE_EFFECT: Int = 0
        const val RESULT: Int = 1
    }
}
