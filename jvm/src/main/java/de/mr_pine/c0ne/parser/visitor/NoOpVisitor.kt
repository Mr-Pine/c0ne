package de.mr_pine.c0ne.parser.visitor

import de.mr_pine.c0ne.parser.ast.*
import de.mr_pine.c0ne.parser.ast.LiteralTree.LiteralBoolTree
import de.mr_pine.c0ne.parser.ast.LiteralTree.LiteralIntTree

/** A visitor that does nothing and returns [Unit] by default.
 * This can be used to implement operations only for specific tree types. */
interface NoOpVisitor<T> : Visitor<T, Unit> {
    override fun visit(assignmentTree: AssignmentTree, data: T) {
    }

    override fun visit(binaryOperationTree: BinaryOperationTree, data: T) {
    }

    override fun visit(blockTree: BlockTree, data: T) {
    }

    override fun visit(declarationTree: DeclarationTree, data: T) {
    }

    override fun visit(functionTree: DeclaredFunctionTree, data: T) {
    }

    override fun visit(identExpressionTree: IdentExpressionTree, data: T) {
    }

    override fun visit(ternaryOperationTree: TernaryOperationTree, data: T) {
    }

    override fun visit(literalIntTree: LiteralIntTree, data: T) {
    }

    override fun visit(literalBoolTree: LiteralBoolTree, data: T) {
    }

    override fun visit(lValueIdentTree: LValueIdentTree, data: T) {
    }

    override fun visit(nameTree: NameTree, data: T) {
    }

    override fun visit(unaryOperationTree: UnaryOperationTree, data: T) {
    }

    override fun visit(programTree: ProgramTree, data: T) {
    }

    override fun visit(ifTree: IfTree, data: T) {
    }

    override fun visit(whileTree: WhileTree, data: T) {
    }

    override fun visit(forTree: ForTree, data: T) {
    }

    override fun visit(breakTree: BreakTree, data: T) {
    }

    override fun visit(continueTree: ContinueTree, data: T) {
    }

    override fun visit(returnTree: ReturnTree, data: T) {
    }

    override fun visit(typeTree: TypeTree, data: T) {
    }

    override fun visit(callTree: CallTree, data: T) {
    }

    override fun visit(builtinFunction: FunctionTree.BuiltinFunction, data: T) {
    }

    override fun <V : Tree> visit(
        parenthesizedListTree: ParenthesizedListTree<V>,
        data: T
    ) {
    }
}
