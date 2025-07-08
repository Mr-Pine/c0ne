package de.mr_pine.c0ne.ir

import de.mr_pine.c0ne.ir.node.*
import de.mr_pine.c0ne.ir.optimize.FinishPassOptimizer
import de.mr_pine.c0ne.ir.optimize.Optimizer
import de.mr_pine.c0ne.ir.util.DebugInfo
import de.mr_pine.c0ne.ir.util.DebugInfo.SourceInfo
import de.mr_pine.c0ne.ir.util.DebugInfoHelper
import de.mr_pine.c0ne.lexer.Operator
import de.mr_pine.c0ne.parser.ast.*
import de.mr_pine.c0ne.parser.ast.LiteralTree.LiteralBoolTree
import de.mr_pine.c0ne.parser.ast.LiteralTree.LiteralIntTree
import de.mr_pine.c0ne.parser.symbol.Name
import de.mr_pine.c0ne.parser.type.BasicType
import de.mr_pine.c0ne.parser.visitor.Visitor
import java.util.*

/** SSA translation as described in
 * [`Simple and Efficient Construction of Static Single Assignment Form`](https://compilers.cs.uni-saarland.de/papers/bbhlmz13cc.pdf).
 *
 * This implementation also tracks side effect edges that can be used to avoid reordering of operations that cannot be
 * reordered.
 *
 * We recommend reading the paper to better understand the mechanics implemented here. */
class SsaTranslation(
    private val function: DeclaredFunctionTree,
    optimizer: Optimizer,
    private val finishPassOptimizer: FinishPassOptimizer
) {
    private val constructor: GraphConstructor = GraphConstructor(optimizer, function.nameTree.name.asString())

    fun translate(): IrGraph {
        val visitor = SsaTranslationVisitor()
        this.function.accept(visitor, this)
        finishPassOptimizer.optimize(this.constructor.graph)
        return this.constructor.graph
    }

    private fun writeVariable(variable: Name, block: Block, value: Node) {
        this.constructor.writeVariable(variable, block, value)
    }

    private fun readVariable(variable: Name, block: Block): Node {
        return this.constructor.readVariable(variable, block)
    }

    private fun currentBlock(): Block {
        return this.constructor.currentBlock
    }

    private class SsaTranslationVisitor : Visitor<SsaTranslation, Node?> {
        private val debugStack: Deque<DebugInfo> = ArrayDeque<DebugInfo>()

        fun pushSpan(tree: Tree) {
            this.debugStack.push(DebugInfoHelper.debugInfo)
            DebugInfoHelper.debugInfo = SourceInfo(tree.span)
        }

        fun popSpan() {
            DebugInfoHelper.debugInfo = this.debugStack.pop()
        }

        override fun visit(assignmentTree: AssignmentTree, data: SsaTranslation): Node? {
            pushSpan(assignmentTree)
            val desugar: ((Node, Node) -> Node)? = when (assignmentTree.operator.type) {
                Operator.OperatorType.ASSIGN_MINUS -> data.constructor::newSub
                Operator.OperatorType.ASSIGN_PLUS -> data.constructor::newAdd
                Operator.OperatorType.ASSIGN_MUL -> data.constructor::newMul
                Operator.OperatorType.ASSIGN_DIV -> { lhs: Node, rhs: Node ->
                    projResultDivMod(
                        data,
                        data.constructor.newDiv(lhs, rhs)
                    )
                }

                Operator.OperatorType.ASSIGN_MOD -> { lhs: Node, rhs: Node ->
                    projResultDivMod(
                        data,
                        data.constructor.newMod(lhs, rhs)
                    )
                }

                Operator.OperatorType.ASSIGN_OR -> data.constructor::newBitwiseOr
                Operator.OperatorType.ASSIGN_AND -> data.constructor::newBitwiseAnd
                Operator.OperatorType.ASSIGN_XOR -> data.constructor::newXor
                Operator.OperatorType.ASSIGN_LEFT_SHIFT -> data.constructor::newLeftShift
                Operator.OperatorType.ASSIGN_RIGHT_SHIFT -> data.constructor::newRightShift

                Operator.OperatorType.ASSIGN -> null
                else -> throw IllegalArgumentException("not an assignment operator " + assignmentTree.operator)
            }

            when (assignmentTree.lValue) {
                is LValueIdentTree -> {
                    var rhs = assignmentTree.expression.accept(this, data)!!
                    if (desugar != null) {
                        rhs = desugar(data.readVariable(assignmentTree.lValue.name.name, data.currentBlock()), rhs)
                    }
                    data.writeVariable(assignmentTree.lValue.name.name, data.currentBlock(), rhs)
                }

                else -> throw IllegalStateException("Unexpected value: " + assignmentTree.lValue)
            }
            popSpan()
            return NOT_AN_EXPRESSION
        }

        override fun visit(binaryOperationTree: BinaryOperationTree, data: SsaTranslation): Node? {
            if (binaryOperationTree.operatorType == Operator.OperatorType.LOGICAL_AND) {
                return shortCircuitLogicalAnd(data, binaryOperationTree)
            } else if (binaryOperationTree.operatorType == Operator.OperatorType.LOGICAL_OR) {
                return shortCircuitLogicalOr(data, binaryOperationTree)
            }

            pushSpan(binaryOperationTree)
            val lhs = binaryOperationTree.lhs.accept(this, data)!!
            val rhs = binaryOperationTree.rhs.accept(this, data)!!
            val res = when (binaryOperationTree.operatorType) {
                Operator.OperatorType.MINUS -> data.constructor.newSub(lhs, rhs)
                Operator.OperatorType.PLUS -> data.constructor.newAdd(lhs, rhs)
                Operator.OperatorType.MUL -> data.constructor.newMul(lhs, rhs)
                Operator.OperatorType.DIV -> projResultDivMod(
                    data,
                    data.constructor.newDiv(lhs, rhs)
                )

                Operator.OperatorType.MOD -> projResultDivMod(
                    data,
                    data.constructor.newMod(lhs, rhs)
                )

                Operator.OperatorType.LEFT_SHIFT -> data.constructor.newLeftShift(lhs, rhs)
                Operator.OperatorType.RIGHT_SHIFT -> data.constructor.newRightShift(lhs, rhs)

                Operator.OperatorType.BITWISE_AND -> data.constructor.newBitwiseAnd(lhs, rhs)
                Operator.OperatorType.BITWISE_XOR -> data.constructor.newXor(lhs, rhs)
                Operator.OperatorType.BITWISE_OR -> data.constructor.newBitwiseOr(lhs, rhs)

                Operator.OperatorType.LESS_THAN -> data.constructor.newLessThan(lhs, rhs)
                Operator.OperatorType.LESS_THAN_OR_EQUAL -> data.constructor.newLessThanOrEqual(lhs, rhs)
                Operator.OperatorType.GREATER_THAN -> data.constructor.newGreaterThan(lhs, rhs)
                Operator.OperatorType.GREATER_THAN_OR_EQUAL -> data.constructor.newGreaterThanOrEqual(lhs, rhs)

                Operator.OperatorType.EQUALS -> {
                    data.constructor.newEquals(lhs, rhs, binaryOperationTree.lhs.type.smallSize)
                }
                Operator.OperatorType.NOT_EQUALS -> {
                    data.constructor.newNotEquals(lhs, rhs, binaryOperationTree.lhs.type.smallSize)
                }

                else -> throw java.lang.IllegalArgumentException("not a binary expression operator " + binaryOperationTree.operatorType)
            }
            popSpan()
            return res
        }

        override fun visit(blockTree: BlockTree, data: SsaTranslation): Node? {
            pushSpan(blockTree)
            for (statement in blockTree.statements) {
                statement.accept(this, data)
                // skip everything after a return in a block
                if (statement is ReturnTree) {
                    break
                }
            }
            popSpan()
            return NOT_AN_EXPRESSION
        }

        override fun visit(declarationTree: DeclarationTree, data: SsaTranslation): Node? {
            pushSpan(declarationTree)
            if (declarationTree.initializer != null) {
                val rhs = declarationTree.initializer.accept(
                    this,
                    data
                )!!
                data.writeVariable(declarationTree.name.name, data.currentBlock(), rhs)
            }
            popSpan()
            return NOT_AN_EXPRESSION
        }

        override fun visit(
            structureTree: StructureTree,
            data: SsaTranslation
        ): Node? {
            error("What is SSA on a struct definition supposed to mean? Why did you call it then?")
        }

        override fun visit(functionTree: DeclaredFunctionTree, data: SsaTranslation): Node? {
            pushSpan(functionTree)
            val start = data.constructor.newStart()
            data.constructor.writeCurrentSideEffect(data.constructor.newSideEffectProj(start))
            for ((i, functionParameter) in functionTree.parameters.elements.withIndex()) {
                val node = data.constructor.newParameterProj(start, functionParameter.name.name, i)
                data.constructor.writeVariable(functionParameter.name.name, data.currentBlock(), node)
            }
            functionTree.body.accept(this, data)
            popSpan()
            return NOT_AN_EXPRESSION
        }

        override fun visit(identExpressionTree: IdentExpressionTree, data: SsaTranslation): Node? {
            pushSpan(identExpressionTree)
            val value = data.readVariable(identExpressionTree.name.name, data.currentBlock())
            popSpan()
            return value
        }

        override fun visit(ternaryOperationTree: TernaryOperationTree, data: SsaTranslation): Node? {
            val condition = ternaryOperationTree.condition.accept(this, data)!!
            val (trueProj, falseProj) = projectedIfNode(data, condition)
            data.constructor.sealBlock(data.constructor.currentBlock)

            val trueBlock = data.constructor.newBlock("ternary-true")
            val falseBlock = data.constructor.newBlock("ternary-false")
            trueBlock.addPredecessor(trueProj)
            falseBlock.addPredecessor(falseProj)
            data.constructor.sealBlock(trueBlock)
            data.constructor.sealBlock(falseBlock)

            data.constructor.currentBlock = trueBlock
            val trueValue = ternaryOperationTree.thenExpression.accept(this, data)!!
            val trueExit = data.constructor.newJump()
            data.constructor.currentBlock = falseBlock
            val falseValue = ternaryOperationTree.elseExpression.accept(this, data)!!
            val falseExit = data.constructor.newJump()

            val followBlock = data.constructor.newBlock("ternary-follow")
            data.constructor.currentBlock = followBlock
            followBlock.addPredecessor(trueExit)
            followBlock.addPredecessor(falseExit)
            data.constructor.sealBlock(followBlock)
            val value = data.constructor.newPhi(data.constructor.currentBlock)
            value.addPredecessor(trueValue)
            value.addPredecessor(falseValue)
            data.constructor.currentBlock = followBlock

            return data.constructor.tryRemoveTrivialPhi(value)
        }

        override fun visit(literalIntTree: LiteralIntTree, data: SsaTranslation): Node? {
            pushSpan(literalIntTree)
            val node = data.constructor.newConstInt(literalIntTree.parseValue()!!.toInt())
            popSpan()
            return node
        }

        override fun visit(literalBoolTree: LiteralBoolTree, data: SsaTranslation): Node? {
            pushSpan(literalBoolTree)
            val node = data.constructor.newConstBool(literalBoolTree.parseValue())
            popSpan()
            return node
        }

        override fun visit(lValueIdentTree: LValueIdentTree, data: SsaTranslation): Node? {
            return NOT_AN_EXPRESSION
        }

        override fun visit(nameTree: NameTree, data: SsaTranslation): Node? {
            return NOT_AN_EXPRESSION
        }

        override fun visit(unaryOperationTree: UnaryOperationTree, data: SsaTranslation): Node? {
            pushSpan(unaryOperationTree)
            val node = unaryOperationTree.expression.accept(
                this,
                data
            )!!
            val res = when (unaryOperationTree.operator.type) {
                Operator.OperatorType.MINUS -> data.constructor.newSub(data.constructor.newConstInt(0), node)
                Operator.OperatorType.LOGICAL_NOT -> data.constructor.newLogicalNot(node)
                Operator.OperatorType.BITWISE_NOT -> data.constructor.newBitwiseNot(node)
                else -> TODO("Unsupported ${unaryOperationTree.operator.type}")
            }
            popSpan()
            return res
        }

        override fun visit(programTree: ProgramTree, data: SsaTranslation): Node? {
            throw UnsupportedOperationException()
        }

        data class IfProjections(val trueProj: ProjNode, val falseProj: ProjNode)

        private fun projectedIfNode(data: SsaTranslation, condition: Node): IfProjections {
            val ifNode = data.constructor.newIf(condition)
            val trueProj = data.constructor.newIfTrueProjection(ifNode)
            val falseProj = data.constructor.newIfFalseProjection(ifNode)
            return IfProjections(trueProj, falseProj)
        }

        override fun visit(ifTree: IfTree, data: SsaTranslation): Node? {
            pushSpan(ifTree)
            val condition = ifTree.condition.accept(this, data)!!

            fun processBranch(branch: StatementTree?, projection: ProjNode, label: String): Node? {
                val block = data.constructor.newBlock("if-body-$label")
                data.constructor.currentBlock = block
                block.addPredecessor(projection)
                data.constructor.sealBlock(block)
                branch?.accept(this, data)
                data.constructor.sealBlock(data.constructor.currentBlock)
                return data.constructor.newJump()
            }

            val (trueProj, falseProj) = projectedIfNode(data, condition)
            data.constructor.sealBlock(data.constructor.currentBlock)

            val trueControl = processBranch(ifTree.thenTree, trueProj, "true")
            val falseControl = processBranch(ifTree.elseTree, falseProj, "false")

            val nextBlock = data.constructor.newBlock("following-if")
            data.constructor.currentBlock = nextBlock
            trueControl?.let { nextBlock.addPredecessor(it) }
            falseControl?.let { nextBlock.addPredecessor(it) }
            data.constructor.sealBlock(nextBlock)

            popSpan()
            return NOT_AN_EXPRESSION
        }

        override fun visit(whileTree: WhileTree, data: SsaTranslation): Node? {
            pushSpan(whileTree)

            data.constructor.sealBlock(data.constructor.currentBlock)
            val exitJump = data.constructor.newJump()

            val whileBlock = data.constructor.newBlock("while")
            whileBlock.addPredecessor(exitJump)
            data.constructor.currentBlock = whileBlock
            data.constructor.pushLoopBlock(whileBlock)
            val condition = whileTree.condition.accept(this, data)!!

            val (trueProj, falseProj) = projectedIfNode(data, condition)

            val followBlock = data.constructor.newBlock("following-while")
            followBlock.addPredecessor(falseProj)
            data.constructor.pushLoopFollow(followBlock)

            val bodyBlock = data.constructor.newBlock("while-body")
            bodyBlock.addPredecessor(trueProj)
            data.constructor.sealBlock(bodyBlock)
            data.constructor.currentBlock = bodyBlock
            whileTree.loopBody.accept(this, data)
            val continueJump = data.constructor.newJump()
            data.constructor.sealBlock(data.constructor.currentBlock)
            whileBlock.addPredecessor(continueJump)

            data.constructor.popLoopBlock(whileBlock)
            data.constructor.sealBlock(whileBlock)
            data.constructor.popLoopFollow(followBlock)
            data.constructor.sealBlock(followBlock)

            data.constructor.currentBlock = followBlock

            popSpan()
            return NOT_AN_EXPRESSION
        }

        override fun visit(forTree: ForTree, data: SsaTranslation): Node? {
            pushSpan(forTree)
            forTree.initializer?.accept(this, data)
            data.constructor.sealBlock(data.constructor.currentBlock)
            val entryJump = data.constructor.newJump()

            val forBlock = data.constructor.newBlock("for")
            forBlock.addPredecessor(entryJump)

            val followBlock = data.constructor.newBlock("following-for")
            val bodyBlock = data.constructor.newBlock("for-body")
            val stepBlock = data.constructor.newBlock("for-step")
            data.constructor.pushLoopBlock(stepBlock)
            data.constructor.pushLoopFollow(followBlock)

            data.constructor.currentBlock = forBlock
            val condition = forTree.condition.accept(this, data)!!
            val (trueProj, falseProj) = projectedIfNode(data, condition)
            bodyBlock.addPredecessor(trueProj)
            followBlock.addPredecessor(falseProj)

            data.constructor.currentBlock = stepBlock
            forTree.step?.accept(this, data)
            val stepExit = data.constructor.newJump()
            forBlock.addPredecessor(stepExit)

            data.constructor.currentBlock = bodyBlock
            forTree.loopBody.accept(this, data)
            val normalLoopExit = data.constructor.newJump()
            stepBlock.addPredecessor(normalLoopExit)


            data.constructor.sealBlock(forBlock)
            data.constructor.sealBlock(followBlock)
            data.constructor.sealBlock(stepBlock)
            data.constructor.sealBlock(bodyBlock)

            data.constructor.popLoopFollow(followBlock)
            data.constructor.popLoopBlock(stepBlock)

            data.constructor.currentBlock = followBlock

            popSpan()
            return NOT_AN_EXPRESSION
        }

        override fun visit(breakTree: BreakTree, data: SsaTranslation): Node? {
            pushSpan(breakTree)
            val breakNode = data.constructor.newJump()
            data.constructor.getLoopFollow().addPredecessor(breakNode)
            data.constructor.sealBlock(data.constructor.currentBlock)
            data.constructor.currentBlock = data.constructor.newBlock("following-break")

            popSpan()
            return NOT_AN_EXPRESSION
        }

        override fun visit(continueTree: ContinueTree, data: SsaTranslation): Node? {
            pushSpan(continueTree)
            val breakNode = data.constructor.newJump()
            data.constructor.getLoopBlock().addPredecessor(breakNode)
            data.constructor.sealBlock(data.constructor.currentBlock)
            data.constructor.currentBlock = data.constructor.newBlock("following-continue")

            popSpan()
            return NOT_AN_EXPRESSION
        }

        override fun visit(returnTree: ReturnTree, data: SsaTranslation): Node? {
            pushSpan(returnTree)
            val node = returnTree.expression.accept(this, data)!!
            val ret = data.constructor.newReturn(node)
            data.constructor.graph.endBlock.addPredecessor(ret)
            val followBlock = data.constructor.newBlock("following-return")
            data.constructor.currentBlock = followBlock
            data.constructor.sealBlock(followBlock)
            popSpan()
            return NOT_AN_EXPRESSION
        }

        override fun visit(typeTree: TypeTree, data: SsaTranslation): Node? {
            throw UnsupportedOperationException()
        }

        override fun visit(callTree: CallTree, data: SsaTranslation): Node? {
            val arguments = buildList {
                for (arg in callTree.arguments.elements) {
                    val argNode = arg.accept(this@SsaTranslationVisitor, data)!!
                    add(argNode)
                }
            }
            val call = data.constructor.newCall(callTree.identifier.name, arguments)
            data.constructor.writeCurrentSideEffect(call)
            return call
        }

        override fun visit(heapAllocationTree: HeapAllocationTree, data: SsaTranslation): Node? {
            TODO("Heap allocation SSA")
        }

        override fun visit(builtinFunction: FunctionTree.BuiltinFunction, data: SsaTranslation): Node? {
            return NOT_AN_EXPRESSION
        }

        override fun visit(
            parameterTree: ParameterTree,
            data: SsaTranslation
        ): Node? {
            return null
        }

        override fun <V : Tree> visit(parenthesizedListTree: ParenthesizedListTree<V>, data: SsaTranslation): Node? {
            error("Plain parenthesized lists are not supported in SSA translation")
        }

        private enum class ShortCircuitType {
            LOGICAL_AND, LOGICAL_OR
        }

        fun shortCircuiting(data: SsaTranslation, tree: BinaryOperationTree, type: ShortCircuitType): Node {
            pushSpan(tree)

            val lhs = tree.lhs.accept(this, data)!!

            val continueCalculationBlock = data.constructor.newBlock("short-circuit-${type.name.lowercase()}-continue")
            val followBlock = data.constructor.newBlock("short-circuit-${type.name.lowercase()}-follow")

            val (trueProj, falseProj) = projectedIfNode(data, lhs)
            continueCalculationBlock.addPredecessor(if (type == ShortCircuitType.LOGICAL_AND) trueProj else falseProj)
            data.constructor.sealBlock(continueCalculationBlock)
            followBlock.addPredecessor(if (type == ShortCircuitType.LOGICAL_AND) falseProj else trueProj)

            data.constructor.currentBlock = continueCalculationBlock
            val rhs = tree.rhs.accept(this, data)!!
            val continueExit = data.constructor.newJump()

            followBlock.addPredecessor(continueExit)
            data.constructor.sealBlock(followBlock)
            data.constructor.currentBlock = followBlock

            val phi = data.constructor.newPhi(data.constructor.currentBlock)
            phi.appendOperand(lhs)
            phi.appendOperand(rhs)

            val res = data.constructor.tryRemoveTrivialPhi(phi)

            popSpan()
            return res
        }

        fun shortCircuitLogicalAnd(data: SsaTranslation, tree: BinaryOperationTree) =
            shortCircuiting(data, tree, ShortCircuitType.LOGICAL_AND)

        fun shortCircuitLogicalOr(data: SsaTranslation, tree: BinaryOperationTree) =
            shortCircuiting(data, tree, ShortCircuitType.LOGICAL_OR)

        fun projResultDivMod(data: SsaTranslation, divMod: Node): Node {
            // make sure we actually have a div or a mod, as optimizations could
            // have changed it to something else already
            if (!(divMod is DivNode || divMod is ModNode)) {
                return divMod
            }
            val projSideEffect = data.constructor.newSideEffectProj(divMod)
            data.constructor.writeCurrentSideEffect(projSideEffect)
            return data.constructor.newResultProj(divMod)
        }

        companion object {
            private val NOT_AN_EXPRESSION: Node? = null
        }
    }
}
