package de.mr_pine.c0ne.ir.visitor

import de.mr_pine.c0ne.ir.node.*

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
    fun visit(node: ModNode): R
    fun visit(node: MulNode): R
    fun visit(node: Phi): R
    fun visit(node: ProjNode): R
    fun visit(node: ReturnNode): R
    fun visit(node: StartNode): R
    fun visit(node: SubNode): R
    fun visit(node: UndefNode): R
    fun visit(node: XorNode): R
    fun visit(node: CallNode): R
    fun visit(node: MemoryReadNode): R
    fun visit(node: MemoryWriteNode): R
}