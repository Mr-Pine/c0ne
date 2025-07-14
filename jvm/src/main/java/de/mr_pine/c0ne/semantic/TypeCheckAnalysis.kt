package de.mr_pine.c0ne.semantic

import de.mr_pine.c0ne.lexer.Operator
import de.mr_pine.c0ne.parser.ast.*
import de.mr_pine.c0ne.parser.symbol.Name
import de.mr_pine.c0ne.parser.type.*
import de.mr_pine.c0ne.parser.type.Type.SmallType
import de.mr_pine.c0ne.parser.type.Type.SmallType.*
import de.mr_pine.c0ne.parser.visitor.NoOpVisitor

class TypeCheckAnalysis : NoOpVisitor<TypeCheckAnalysis.TypeData> {
    class TypeData(
        val returns: MutableList<ReturnTree> = mutableListOf(), private val structDeclarations: Map<Name, StructureTree>
    ) {
        fun getStruct(name: Name) = structDeclarations[name]
        fun resolve(type: Type): Type {
            when (type) {
                is StructType -> {
                    type.references =
                        structDeclarations[type.name] ?: throw SemanticException("Unknown struct type $type")
                }

                else -> {}
            }
            return type
        }
    }

    override fun visit(functionTree: DeclaredFunctionTree, data: TypeData) {
        for (returnTree in data.returns) {
            if (data.resolve(returnTree.expression.type) incompatibleWith data.resolve(functionTree.returnType)) throw SemanticException(
                "Return type ${returnTree.expression.type} at ${returnTree.span} does not match expected type ${functionTree.returnType}"
            )
        }
        data.returns.clear()

        super.visit(functionTree, data)
    }

    override fun visit(returnTree: ReturnTree, data: TypeData) {
        data.returns.add(returnTree)
        super.visit(returnTree, data)
    }

    override fun visit(
        declarationTree: DeclarationTree, data: TypeData
    ) {
        if (declarationTree.initializer != null) {
            if (declarationTree.initializer.type incompatibleWith declarationTree.type) throw SemanticException("Type mismatch at ${declarationTree.span} for ${declarationTree.name.name} initializer: Expected ${declarationTree.type} got ${declarationTree.initializer.type}")
        }
        super.visit(declarationTree, data)
    }

    override fun visit(
        assignmentTree: AssignmentTree, data: TypeData
    ) {
        val lType = assignmentTree.lValue.type
        if (assignmentTree.expression.type incompatibleWith lType) throw SemanticException("Type mismatch at ${assignmentTree.span} for ${assignmentTree.lValue}: Expected $lType got ${assignmentTree.expression.type}")
        if (assignmentTree.operator.type != Operator.OperatorType.ASSIGN) {
            val operatorType = assignmentTree.operator.type.inputType
            if (lType incompatibleWith operatorType!!) throw SemanticException("Type mismatch at ${assignmentTree.span}: Operator ${assignmentTree.operator.type} expects $operatorType but got $lType")
        }
        super.visit(assignmentTree, data)
    }

    override fun visit(
        binaryOperationTree: BinaryOperationTree, data: TypeData
    ) {
        val lhsType = binaryOperationTree.lhs.type
        val rhsType = binaryOperationTree.rhs.type

        if (lhsType !is SmallType) {
            throw SemanticException("Type mismatch at ${binaryOperationTree.span} for ${binaryOperationTree.lhs}: Expected small type got $lhsType")
        }

        val inputType = binaryOperationTree.operatorType.inputType

        if (inputType != null) {
            if (lhsType incompatibleWith inputType) throw SemanticException("Type mismatch at ${binaryOperationTree.span} for ${binaryOperationTree.lhs}: Expected $inputType got $lhsType")
            if (rhsType incompatibleWith inputType) throw SemanticException("Type mismatch at ${binaryOperationTree.span} for ${binaryOperationTree.rhs}: Expected $inputType got $rhsType")
        }

        val commonType = Type.commonType(lhsType, rhsType)
        if (commonType == TypeError) throw SemanticException("Type mismatch at ${binaryOperationTree.span}: Could not unify types $lhsType and $rhsType")

        super.visit(binaryOperationTree, data)
    }

    override fun visit(
        unaryOperationTree: UnaryOperationTree, data: TypeData
    ) {
        val expressionType = unaryOperationTree.expression.type
        if (expressionType incompatibleWith unaryOperationTree.operator.type.inputType!!) throw SemanticException("Type mismatch at ${unaryOperationTree.span} for ${unaryOperationTree.expression}: Expected ${unaryOperationTree.operator.type.inputType} got $expressionType")
        super.visit(unaryOperationTree, data)
    }

    override fun visit(ifTree: IfTree, data: TypeData) {
        if (ifTree.condition.type incompatibleWith BasicType.Boolean) throw SemanticException("Type mismatch at ${ifTree.span} for if condition: Expected ${BasicType.Boolean} got ${ifTree.condition.type}")

        super.visit(ifTree, data)
    }

    override fun visit(whileTree: WhileTree, data: TypeData) {
        if (whileTree.condition.type incompatibleWith BasicType.Boolean) throw SemanticException("Type mismatch at ${whileTree.span} for while condition: Expected ${BasicType.Boolean} got ${whileTree.condition.type}")

        super.visit(whileTree, data)
    }

    override fun visit(forTree: ForTree, data: TypeData) {
        if (forTree.condition.type incompatibleWith BasicType.Boolean) throw SemanticException("Type mismatch at ${forTree.span} for condition of for loop: Expected ${BasicType.Boolean} got ${forTree.condition.type}")

        super.visit(forTree, data)
    }

    override fun visit(
        callTree: CallTree, data: TypeData
    ) {
        for ((argument, parameterType) in callTree.arguments.elements.zip(callTree.references!!.parameterTypes)) {
            if (argument.type incompatibleWith parameterType) throw SemanticException("Type mismatch at ${argument.span} for argument ${argument.type} in call to ${callTree.references!!.name} at ${callTree.span}")
        }
    }

    override fun visit(
        dereferenceTree: DereferenceTree, data: TypeData
    ) {
        val pointerType = data.resolve(dereferenceTree.pointerValue.type)
        if (pointerType !is PointerType) throw SemanticException("Type mismatch at ${dereferenceTree.span} for dereference of ${dereferenceTree.pointerValue.type}: Expected pointer type got $pointerType")
    }

    override fun visit(
        fieldAccessTree: FieldAccessTree, data: TypeData
    ) {
        val structType = data.resolve(fieldAccessTree.structValue.type)
        if (structType !is StructType) throw SemanticException("Type mismatch at ${fieldAccessTree.span} for field access of ${fieldAccessTree.structValue.type}: Expected struct type got $structType")
    }

    override fun visit(
        arrayAccessTree: ArrayAccessTree, data: TypeData
    ) {
        val arrayType = data.resolve(arrayAccessTree.arrayValue.type)
        if (arrayType !is ArrayType) throw SemanticException("Type mismatch at ${arrayAccessTree.span} for array access of ${arrayAccessTree.arrayValue.type}: Expected array type got $arrayType")
    }

    override fun visit(
        ternaryOperationTree: TernaryOperationTree, data: TypeData
    ) {
        if (ternaryOperationTree.condition.type incompatibleWith BasicType.Boolean) throw SemanticException("Type mismatch at ${ternaryOperationTree.span} for condition of ternary operation: Expected ${BasicType.Boolean} got ${ternaryOperationTree.condition.type}")

        if (ternaryOperationTree.type == TypeError) throw SemanticException(
            "Type mismatch at ${ternaryOperationTree.span} for then and else expression of ternary operation: No common type of ${ternaryOperationTree.thenExpression.type} got ${ternaryOperationTree.elseExpression.type}"
        )

        super.visit(
            ternaryOperationTree, data
        )
    }

    override fun visit(
        typeTree: TypeTree, data: TypeData
    ) {
        if (typeTree.type is StructType) {
            typeTree.type.references = data.getStruct(typeTree.type.name)
                ?: throw SemanticException("Unknown struct ${typeTree.type.name} at ${typeTree.span}")
        }
        super.visit(typeTree, data)
    }
}