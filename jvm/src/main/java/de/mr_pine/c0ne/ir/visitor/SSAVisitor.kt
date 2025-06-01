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

interface SSAVisitor<R> {
    fun visit(node: AddNode): R
    fun visit(node: ArithmeticLeftShiftNode): R
    fun visit(node: ArithmeticRightShiftNode): R
    fun visit(node: BitwiseAndNode): R
    fun visit(node: BitwiseNotNode): R
    fun visit(node: BitwiseOrNode): R
    fun visit(node: Block): R
    fun visit(node: ConstBoolNode): R
    fun visit(node: ConstIntNode): R
    fun visit(node: DivNode): R
    fun visit(node: EqualsNode): R
    fun visit(node: IfNode): R
    fun visit(node: JumpNode): R
    fun visit(node: LessThanEqNode): R
    fun visit(node: LessThanNode): R
    fun visit(node: LogicalAndNode): R
    fun visit(node: LogicalOrNode): R
    fun visit(node: ModNode): R
    fun visit(node: MulNode): R
    fun visit(node: NotNode): R
    fun visit(node: Phi): R
    fun visit(node: ProjNode): R
    fun visit(node: ReturnNode): R
    fun visit(node: StartNode): R
    fun visit(node: SubNode): R
    fun visit(node: UndefNode): R
    fun visit(node: XorNode): R
}