package de.mr_pine.c0ne.ir.node

class IfNode(block: Block, condition: Node): Node(block, condition), ControlNode {
}