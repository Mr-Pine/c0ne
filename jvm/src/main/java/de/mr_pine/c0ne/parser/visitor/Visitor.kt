package de.mr_pine.c0ne.parser.visitor

import de.mr_pine.c0ne.parser.ast.*
import de.mr_pine.c0ne.parser.ast.LiteralTree.LiteralBoolTree
import de.mr_pine.c0ne.parser.ast.LiteralTree.LiteralIntTree

interface Visitor<T, R> {
    fun visit(assignmentTree: AssignmentTree, data: T): R

    fun visit(binaryOperationTree: BinaryOperationTree, data: T): R

    fun visit(blockTree: BlockTree, data: T): R

    fun visit(declarationTree: DeclarationTree, data: T): R

    fun visit(functionTree: DeclaredFunctionTree, data: T): R

    fun visit(structureTree: StructureTree, data: T): R

    fun visit(identExpressionTree: IdentExpressionTree, data: T): R

    fun visit(ternaryOperationTree: TernaryOperationTree, data: T): R

    fun visit(literalIntTree: LiteralIntTree, data: T): R

    fun visit(literalBoolTree: LiteralBoolTree, data: T): R

    fun visit(lValueIdentTree: LValueIdentTree, data: T): R

    fun visit(nameTree: NameTree, data: T): R

    fun visit(unaryOperationTree: UnaryOperationTree, data: T): R

    fun visit(programTree: ProgramTree, data: T): R

    fun visit(ifTree: IfTree, data: T): R

    fun visit(whileTree: WhileTree, data: T): R

    fun visit(forTree: ForTree, data: T): R

    fun visit(breakTree: BreakTree, data: T): R

    fun visit(continueTree: ContinueTree, data: T): R

    fun visit(returnTree: ReturnTree, data: T): R

    fun visit(typeTree: TypeTree, data: T): R

    fun visit(callTree: CallTree, data: T): R

    fun visit(builtinFunction: FunctionTree.BuiltinFunction, data: T): R

    fun visit(parameterTree: ParameterTree, data: T): R

    fun <V: Tree> visit(parenthesizedListTree: ParenthesizedListTree<V>, data: T): R
}
