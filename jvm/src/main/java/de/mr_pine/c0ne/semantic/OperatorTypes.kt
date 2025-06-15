package de.mr_pine.c0ne.semantic

import de.mr_pine.c0ne.lexer.Operator
import de.mr_pine.c0ne.parser.type.BasicType
import de.mr_pine.c0ne.parser.type.Type

val Operator.OperatorType.outType
    get() = when (this) {
        Operator.OperatorType.LOGICAL_NOT, Operator.OperatorType.LOGICAL_AND, Operator.OperatorType.LOGICAL_OR, Operator.OperatorType.LESS_THAN, Operator.OperatorType.LESS_THAN_OR_EQUAL, Operator.OperatorType.GREATER_THAN, Operator.OperatorType.GREATER_THAN_OR_EQUAL, Operator.OperatorType.EQUALS, Operator.OperatorType.NOT_EQUALS -> BasicType.Boolean

        Operator.OperatorType.BITWISE_NOT, Operator.OperatorType.MINUS, Operator.OperatorType.MUL, Operator.OperatorType.DIV, Operator.OperatorType.MOD, Operator.OperatorType.PLUS, Operator.OperatorType.LEFT_SHIFT, Operator.OperatorType.RIGHT_SHIFT, Operator.OperatorType.BITWISE_AND, Operator.OperatorType.BITWISE_XOR, Operator.OperatorType.BITWISE_OR -> BasicType.Integer

        Operator.OperatorType.TERNARY_QUESTION, Operator.OperatorType.TERNARY_COLON, Operator.OperatorType.ASSIGN, Operator.OperatorType.ASSIGN_PLUS, Operator.OperatorType.ASSIGN_MINUS, Operator.OperatorType.ASSIGN_MUL, Operator.OperatorType.ASSIGN_DIV, Operator.OperatorType.ASSIGN_MOD, Operator.OperatorType.ASSIGN_AND, Operator.OperatorType.ASSIGN_XOR, Operator.OperatorType.ASSIGN_OR, Operator.OperatorType.ASSIGN_LEFT_SHIFT, Operator.OperatorType.ASSIGN_RIGHT_SHIFT -> throw Exception(
            "Determining output type for assignment $this is not supported"
        )
    }

val Operator.OperatorType.inputType: Type?
    get() = when (this) {
        Operator.OperatorType.EQUALS, Operator.OperatorType.NOT_EQUALS -> null

        Operator.OperatorType.LOGICAL_NOT, Operator.OperatorType.LOGICAL_AND, Operator.OperatorType.LOGICAL_OR -> BasicType.Boolean

        Operator.OperatorType.BITWISE_NOT, Operator.OperatorType.MINUS, Operator.OperatorType.MUL, Operator.OperatorType.DIV, Operator.OperatorType.MOD, Operator.OperatorType.PLUS, Operator.OperatorType.LEFT_SHIFT, Operator.OperatorType.RIGHT_SHIFT, Operator.OperatorType.BITWISE_AND, Operator.OperatorType.BITWISE_XOR, Operator.OperatorType.BITWISE_OR, Operator.OperatorType.LESS_THAN, Operator.OperatorType.LESS_THAN_OR_EQUAL, Operator.OperatorType.GREATER_THAN, Operator.OperatorType.GREATER_THAN_OR_EQUAL -> BasicType.Integer

        Operator.OperatorType.ASSIGN_PLUS -> Operator.OperatorType.PLUS.inputType
        Operator.OperatorType.ASSIGN_MINUS -> Operator.OperatorType.MINUS.inputType
        Operator.OperatorType.ASSIGN_MUL -> Operator.OperatorType.MUL.inputType
        Operator.OperatorType.ASSIGN_DIV -> Operator.OperatorType.DIV.inputType
        Operator.OperatorType.ASSIGN_MOD -> Operator.OperatorType.MOD.inputType
        Operator.OperatorType.ASSIGN_AND -> Operator.OperatorType.BITWISE_AND.inputType
        Operator.OperatorType.ASSIGN_XOR -> Operator.OperatorType.BITWISE_XOR.inputType
        Operator.OperatorType.ASSIGN_OR -> Operator.OperatorType.BITWISE_OR.inputType
        Operator.OperatorType.ASSIGN_LEFT_SHIFT -> Operator.OperatorType.LEFT_SHIFT.inputType
        Operator.OperatorType.ASSIGN_RIGHT_SHIFT -> Operator.OperatorType.RIGHT_SHIFT.inputType

        Operator.OperatorType.TERNARY_QUESTION, Operator.OperatorType.TERNARY_COLON, Operator.OperatorType.ASSIGN -> throw Exception(
            "Determining input type for assignment $this is not supported"
        )
    }