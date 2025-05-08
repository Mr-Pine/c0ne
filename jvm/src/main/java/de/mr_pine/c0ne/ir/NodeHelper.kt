package de.mr_pine.c0ne.ir

import edu.kit.kastel.vads.compiler.ir.node.BinaryOperationNode
import edu.kit.kastel.vads.compiler.ir.node.DivNode
import edu.kit.kastel.vads.compiler.ir.node.ModNode
import edu.kit.kastel.vads.compiler.ir.node.Node
import edu.kit.kastel.vads.compiler.ir.node.ReturnNode
import edu.kit.kastel.vads.compiler.ir.util.NodeSupport

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