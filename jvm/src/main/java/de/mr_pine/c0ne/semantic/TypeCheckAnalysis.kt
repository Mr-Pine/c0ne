package de.mr_pine.c0ne.semantic

import de.mr_pine.c0ne.lexer.Operator
import de.mr_pine.c0ne.parser.ast.AssignmentTree
import de.mr_pine.c0ne.parser.ast.BinaryOperationTree
import de.mr_pine.c0ne.parser.ast.DeclarationTree
import de.mr_pine.c0ne.parser.ast.ForTree
import de.mr_pine.c0ne.parser.ast.DeclaredFunctionTree
import de.mr_pine.c0ne.parser.ast.IfTree
import de.mr_pine.c0ne.parser.ast.LValueIdentTree
import de.mr_pine.c0ne.parser.ast.ReturnTree
import de.mr_pine.c0ne.parser.ast.TernaryOperationTree
import de.mr_pine.c0ne.parser.ast.UnaryOperationTree
import de.mr_pine.c0ne.parser.ast.WhileTree
import de.mr_pine.c0ne.parser.type.BasicType
import de.mr_pine.c0ne.parser.visitor.NoOpVisitor

class TypeCheckAnalysis : NoOpVisitor<MutableList<ReturnTree>> {
    override fun visit(functionTree: DeclaredFunctionTree, data: MutableList<ReturnTree>) {
        for (returnTree in data) {
            if (returnTree.expression.type != functionTree.returnType) throw SemanticException("Return type ${returnTree.expression.type} at ${returnTree.span} does not match expected type ${functionTree.returnType}")
        }

        super.visit(functionTree, data)
    }

    override fun visit(returnTree: ReturnTree, data: MutableList<ReturnTree>) {
        data.add(returnTree)
        super.visit(returnTree, data)
    }

    override fun visit(
        declarationTree: DeclarationTree, data: MutableList<ReturnTree>
    ) {
        if (declarationTree.initializer != null) {
            if (declarationTree.initializer.type != declarationTree.type) throw SemanticException("Type mismatch at ${declarationTree.span} for ${declarationTree.name.name} initializer: Expected ${declarationTree.type} got ${declarationTree.initializer.type}")
        }
        super.visit(declarationTree, data)
    }

    override fun visit(
        assignmentTree: AssignmentTree, data: MutableList<ReturnTree>
    ) {
        val variableType = (assignmentTree.lValue as LValueIdentTree).references!!.type
        if (assignmentTree.expression.type != variableType) throw SemanticException("Type mismatch at ${assignmentTree.span} for ${assignmentTree.lValue.name.name}: Expected $variableType got ${assignmentTree.expression.type}")
        if (assignmentTree.operator.type != Operator.OperatorType.ASSIGN) {
            val operatorType = assignmentTree.operator.type.inputType
            if (variableType != operatorType) throw SemanticException("Type mismatch at ${assignmentTree.span}: Operator ${assignmentTree.operator.type} expects $operatorType but got $variableType")
        }
        super.visit(assignmentTree, data)
    }

    override fun visit(
        binaryOperationTree: BinaryOperationTree, data: MutableList<ReturnTree>
    ) {
        val lhsType = binaryOperationTree.lhs.type
        val rhsType = binaryOperationTree.rhs.type

        val inputType = binaryOperationTree.operatorType.inputType

        if (inputType != null) {
            if (lhsType != inputType) throw SemanticException("Type mismatch at ${binaryOperationTree.span} for ${binaryOperationTree.lhs}: Expected $inputType got $lhsType")
            if (rhsType != inputType) throw SemanticException("Type mismatch at ${binaryOperationTree.span} for ${binaryOperationTree.rhs}: Expected $inputType got $rhsType")
        }

        if (lhsType != rhsType) throw SemanticException("Type mismatch at ${binaryOperationTree.span}: Could not unify types $lhsType and $rhsType")

        super.visit(binaryOperationTree, data)
    }

    override fun visit(
        unaryOperationTree: UnaryOperationTree, data: MutableList<ReturnTree>
    ) {
        val expressionType = unaryOperationTree.expression.type
        if (unaryOperationTree.operator.type.inputType != expressionType) throw SemanticException("Type mismatch at ${unaryOperationTree.span} for ${unaryOperationTree.expression}: Expected ${unaryOperationTree.operator.type.inputType} got $expressionType")
        super.visit(unaryOperationTree, data)
    }

    override fun visit(ifTree: IfTree, data: MutableList<ReturnTree>) {
        if (ifTree.condition.type != BasicType.Boolean) throw SemanticException("Type mismatch at ${ifTree.span} for if condition: Expected ${BasicType.Boolean} got ${ifTree.condition.type}")

        super.visit(ifTree, data)
    }

    override fun visit(whileTree: WhileTree, data: MutableList<ReturnTree>) {
        if (whileTree.condition.type != BasicType.Boolean) throw SemanticException("Type mismatch at ${whileTree.span} for while condition: Expected ${BasicType.Boolean} got ${whileTree.condition.type}")

        super.visit(whileTree, data)
    }

    override fun visit(forTree: ForTree, data: MutableList<ReturnTree>) {
        if (forTree.condition.type != BasicType.Boolean) throw SemanticException("Type mismatch at ${forTree.span} for condition of for loop: Expected ${BasicType.Boolean} got ${forTree.condition.type}")

        super.visit(forTree, data)
    }

    override fun visit(
        ternaryOperationTree: TernaryOperationTree,
        data: MutableList<ReturnTree>
    ) {
        if (ternaryOperationTree.condition.type != BasicType.Boolean) throw SemanticException("Type mismatch at ${ternaryOperationTree.span} for condition of ternary operation: Expected ${BasicType.Boolean} got ${ternaryOperationTree.condition.type}")

        if (ternaryOperationTree.thenExpression.type != ternaryOperationTree.elseExpression.type) throw SemanticException("Type mismatch at ${ternaryOperationTree.span} for then and else expression of ternary operation: Expected ${ternaryOperationTree.thenExpression.type} got ${ternaryOperationTree.elseExpression.type}")

        super.visit(
            ternaryOperationTree,
            data
        )
    }
}