package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.lexer.Operator
import de.mr_pine.c0ne.parser.type.BasicType
import de.mr_pine.c0ne.parser.visitor.Visitor

data class BinaryOperationTree(
    val lhs: ExpressionTree, val rhs: ExpressionTree, val operatorType: Operator.OperatorType
) : ExpressionTree {
    override val span = this.lhs.span merge this.rhs.span

    override val type = when (operatorType) {
        Operator.OperatorType.LOGICAL_NOT, Operator.OperatorType.LOGICAL_AND, Operator.OperatorType.LOGICAL_OR, Operator.OperatorType.EQUALS, Operator.OperatorType.NOT_EQUALS -> BasicType.Boolean

        Operator.OperatorType.BITWISE_NOT, Operator.OperatorType.MINUS, Operator.OperatorType.MUL, Operator.OperatorType.DIV, Operator.OperatorType.MOD, Operator.OperatorType.PLUS, Operator.OperatorType.LEFT_SHIFT, Operator.OperatorType.RIGHT_SHIFT, Operator.OperatorType.LESS_THAN, Operator.OperatorType.LESS_THAN_OR_EQUAL, Operator.OperatorType.GREATER_THAN, Operator.OperatorType.GREATER_THAN_OR_EQUAL, Operator.OperatorType.BITWISE_AND, Operator.OperatorType.BITWISE_XOR, Operator.OperatorType.BITWISE_OR -> BasicType.Integer

        Operator.OperatorType.TERNARY_QUESTION, Operator.OperatorType.TERNARY_COLON, Operator.OperatorType.ASSIGN, Operator.OperatorType.ASSIGN_PLUS, Operator.OperatorType.ASSIGN_MINUS, Operator.OperatorType.ASSIGN_MUL, Operator.OperatorType.ASSIGN_DIV, Operator.OperatorType.ASSIGN_MOD, Operator.OperatorType.ASSIGN_AND, Operator.OperatorType.ASSIGN_XOR, Operator.OperatorType.ASSIGN_OR, Operator.OperatorType.ASSIGN_LEFT_SHIFT, Operator.OperatorType.ASSIGN_RIGHT_SHIFT -> throw Exception(
            "Determining binop type for assignment is not supported"
        )
    }

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }
}
