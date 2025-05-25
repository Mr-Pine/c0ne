package de.mr_pine.c0ne.ir

import de.mr_pine.c0ne.ir.node.BinaryOperationNode
import de.mr_pine.c0ne.ir.node.DivNode
import de.mr_pine.c0ne.ir.node.ModNode
import de.mr_pine.c0ne.ir.node.Node
import de.mr_pine.c0ne.ir.node.ReturnNode
import de.mr_pine.c0ne.ir.util.NodeSupport

val ReturnNode.result: Node
    get() = NodeSupport.predecessorSkipProj(this, ReturnNode.RESULT)

val BinaryOperationNode.left: Node
    get() = NodeSupport.predecessorSkipProj(this, BinaryOperationNode.LEFT)
val BinaryOperationNode.right: Node
    get() = NodeSupport.predecessorSkipProj(this, BinaryOperationNode.RIGHT)

val DivNode.sideEffect: Node
    get() = NodeSupport.predecessorSkipProj(this, DivNode.SIDE_EFFECT)
val ModNode.sideEffect: Node
    get() = NodeSupport.predecessorSkipProj(this, ModNode.SIDE_EFFECT)