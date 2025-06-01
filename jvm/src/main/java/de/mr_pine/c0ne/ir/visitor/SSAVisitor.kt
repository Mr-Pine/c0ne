package de.mr_pine.c0ne.ir.visitor

import de.mr_pine.c0ne.ir.node.AddNode
import de.mr_pine.c0ne.ir.node.ArithmeticLeftShiftNode
import de.mr_pine.c0ne.ir.node.ArithmeticRightShiftNode
import de.mr_pine.c0ne.ir.node.BitwiseAndNode
import de.mr_pine.c0ne.ir.node.BitwiseNotNode
import de.mr_pine.c0ne.ir.node.BitwiseOrNode
import de.mr_pine.c0ne.ir.node.Block
import de.mr_pine.c0ne.ir.node.ConstBoolNode
import de.mr_pine.c0ne.ir.node.ConstIntNode
import de.mr_pine.c0ne.ir.node.DivNode
import de.mr_pine.c0ne.ir.node.EqualsNode
import de.mr_pine.c0ne.ir.node.IfNode
import de.mr_pine.c0ne.ir.node.JumpNode
import de.mr_pine.c0ne.ir.node.LessThanEqNode
import de.mr_pine.c0ne.ir.node.LessThanNode
import de.mr_pine.c0ne.ir.node.LogicalAndNode
import de.mr_pine.c0ne.ir.node.LogicalOrNode
import de.mr_pine.c0ne.ir.node.ModNode
import de.mr_pine.c0ne.ir.node.MulNode
import de.mr_pine.c0ne.ir.node.NotNode
import de.mr_pine.c0ne.ir.node.Phi
import de.mr_pine.c0ne.ir.node.ProjNode
import de.mr_pine.c0ne.ir.node.ReturnNode
import de.mr_pine.c0ne.ir.node.StartNode
import de.mr_pine.c0ne.ir.node.SubNode
import de.mr_pine.c0ne.ir.node.UndefNode
import de.mr_pine.c0ne.ir.node.XorNode

interface SSAVisitor {
    fun visit(node: AddNode)
    fun visit(node: ArithmeticLeftShiftNode)
    fun visit(node: ArithmeticRightShiftNode)
    fun visit(node: BitwiseAndNode)
    fun visit(node: BitwiseNotNode)
    fun visit(node: BitwiseOrNode)
    fun visit(node: Block)
    fun visit(node: ConstBoolNode)
    fun visit(node: ConstIntNode)
    fun visit(node: DivNode)
    fun visit(node: EqualsNode)
    fun visit(node: IfNode)
    fun visit(node: JumpNode)
    fun visit(node: LessThanEqNode)
    fun visit(node: LessThanNode)
    fun visit(node: LogicalAndNode)
    fun visit(node: LogicalOrNode)
    fun visit(node: ModNode)
    fun visit(node: MulNode)
    fun visit(node: NotNode)
    fun visit(node: Phi)
    fun visit(node: ProjNode)
    fun visit(node: ReturnNode)
    fun visit(node: StartNode)
    fun visit(node: SubNode)
    fun visit(node: UndefNode)
    fun visit(node: XorNode)
}