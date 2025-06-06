package de.mr_pine.c0ne.ir.node

sealed class ExitNode(block: Block, vararg predecessors: Node): Node(block, *predecessors)