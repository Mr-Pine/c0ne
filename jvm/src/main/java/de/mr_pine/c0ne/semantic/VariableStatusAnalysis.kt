package de.mr_pine.c0ne.semantic

import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.parser.ast.*
import de.mr_pine.c0ne.parser.symbol.Name
import de.mr_pine.c0ne.parser.visitor.Visitor

/** Checks that variables are
 * - declared before assignment
 * - not declared twice
 * - not initialized twice
 * - assigned before referenced */
class VariableStatusAnalysis : Visitor<VariableStatusAnalysis.VariableStatus, VariableStatusAnalysis.VariableStatus> {

    override fun visit(
        assignmentTree: AssignmentTree, data: VariableStatus
    ): VariableStatus {
        val status = assignmentTree.expression.accept(this, data)
        if (assignmentTree.lValue !is LValueIdentTree) {
            throw NotImplementedError("Only assignments to variables are currently supported")
        }
        if (assignmentTree.operator.type.isSelfAssignOperator) {
            assignmentTree.lValue.references = data.checkUsage(assignmentTree.lValue.name, assignmentTree.span)
        }
        return status.addDefinition(
            VariableDefinition(assignmentTree.lValue.name.name),
            assignmentTree.span,
            assignmentTree
        )
    }

    override fun visit(
        blockTree: BlockTree, data: VariableStatus
    ): VariableStatus {
        var status = data.enterNewScope()
        for (statement in blockTree.statements) {
            status = statement.accept(this, status)
        }
        return status.exitScope()
    }

    override fun visit(
        binaryOperationTree: BinaryOperationTree, data: VariableStatus
    ): VariableStatus {
        var data = binaryOperationTree.lhs.accept(this, data)
        data = binaryOperationTree.rhs.accept(this, data)
        return data
    }

    override fun visit(
        declarationTree: DeclarationTree, data: VariableStatus
    ): VariableStatus {
        var status = data.addDeclaration(VariableDeclaration(declarationTree.name.name, declarationTree))
        if (declarationTree.initializer != null) {
            status = declarationTree.initializer.accept(this, status)
            status = status.addDefinition(VariableDefinition(declarationTree.name.name), declarationTree.span, null)
        }
        return status
    }

    override fun visit(
        functionTree: DeclaredFunctionTree, data: VariableStatus
    ): VariableStatus {
        var status = data.enterNewScope()
        status = functionTree.parameters.accept(this, status)
        return functionTree.body.accept(this, status).exitScopeWithoutDefs()
    }

    override fun visit(
        structureTree: StructureTree,
        data: VariableStatus
    ): VariableStatus {
        error("Should not be called for structures. Only functions should be visited here.")
    }

    override fun visit(
        identExpressionTree: IdentExpressionTree, data: VariableStatus
    ): VariableStatus {
        identExpressionTree.references = data.checkUsage(identExpressionTree.name, identExpressionTree.span)
        return data
    }

    override fun visit(
        ternaryOperationTree: TernaryOperationTree, data: VariableStatus
    ): VariableStatus {
        var status = ternaryOperationTree.condition.accept(this, data)
        status = ternaryOperationTree.thenExpression.accept(this, status)
        status = ternaryOperationTree.elseExpression.accept(this, status)
        return status
    }

    override fun visit(
        literalIntTree: LiteralTree.LiteralIntTree, data: VariableStatus
    ): VariableStatus {
        return data
    }

    override fun visit(
        literalBoolTree: LiteralTree.LiteralBoolTree, data: VariableStatus
    ): VariableStatus {
        return data
    }

    override fun visit(
        lValueIdentTree: LValueIdentTree, data: VariableStatus
    ): VariableStatus {
        return data
    }

    override fun visit(
        nameTree: NameTree, data: VariableStatus
    ): VariableStatus {
        return data
    }

    override fun visit(
        unaryOperationTree: UnaryOperationTree, data: VariableStatus
    ): VariableStatus {
        return unaryOperationTree.expression.accept(this, data)
    }

    override fun visit(
        programTree: ProgramTree, data: VariableStatus
    ): VariableStatus {
        var status = data
        for (function in programTree.functions) {
            status = function.accept(this, status)
        }
        return status
    }

    override fun visit(
        ifTree: IfTree, data: VariableStatus
    ): VariableStatus {
        val status = ifTree.condition.accept(this, data)
        val afterThenStatus = ifTree.thenTree.accept(this, status.enterNewScope()).exitScope()
        val afterElseStatus = ifTree.elseTree?.accept(this, status.enterNewScope())?.exitScope()

        return afterElseStatus?.intersectLastDefinitions(afterThenStatus) ?: status
    }

    override fun visit(
        whileTree: WhileTree, data: VariableStatus
    ): VariableStatus {
        var status = whileTree.condition.accept(this, data)
        status = status.enterNewScope()
        status = whileTree.loopBody.accept(this, status)
        return status.exitScopeWithoutDefs()
    }

    override fun visit(
        forTree: ForTree, data: VariableStatus
    ): VariableStatus {
        var status = data.enterNewScope()
        status = forTree.initializer?.accept(this, status) ?: status
        status = forTree.condition.accept(this, status)
        status = status.enterNewScope().enterNewScope()
        status = forTree.loopBody.accept(this, status)
        status = status.exitScope()
        status = forTree.step?.accept(this, status) ?: status
        return status.exitScopeWithoutDefs().exitScope()
    }

    override fun visit(
        breakTree: BreakTree, data: VariableStatus
    ): VariableStatus {
        return data.defineAllUndefined()
    }

    override fun visit(
        continueTree: ContinueTree, data: VariableStatus
    ): VariableStatus {
        return data.defineAllUndefined()
    }

    override fun visit(
        returnTree: ReturnTree, data: VariableStatus
    ): VariableStatus {
        val status = returnTree.expression.accept(this, data)
        return status.defineAllUndefined()
    }

    override fun visit(
        typeTree: TypeTree, data: VariableStatus
    ): VariableStatus {
        return data
    }

    override fun visit(
        callTree: CallTree,
        data: VariableStatus
    ): VariableStatus {
        return callTree.arguments.accept(this, data)
    }

    override fun visit(
        builtinFunction: FunctionTree.BuiltinFunction,
        data: VariableStatus
    ): VariableStatus {
        return data
    }

    override fun visit(
        parameterTree: ParameterTree,
        data: VariableStatus
    ): VariableStatus {
        return data.addDeclaration(VariableDeclaration(parameterTree.name.name, parameterTree)).addDefinition(
            VariableDefinition(parameterTree.name.name), parameterTree.span, null
        )
    }

    override fun <V : Tree> visit(
        parenthesizedListTree: ParenthesizedListTree<V>,
        data: VariableStatus
    ): VariableStatus {
        var status = data
        for (elem in parenthesizedListTree.elements) {
            status = elem.accept(this, status)
        }
        return status
    }

    data class VariableDeclaration(val name: Name, val declaration: Declaration)
    data class VariableDefinition(val name: Name)

    private data class ScopeStatus(
        val declarations: Set<VariableDeclaration>, val definitions: Set<VariableDefinition>
    )

    @ConsistentCopyVisibility
    data class VariableStatus private constructor(
        private val scopes: List<ScopeStatus>
    ) {
        private fun declarationFor(name: Name) =
            scopes.flatMap { it.declarations.filter { it.name == name } }.lastOrNull()

        private fun definitionFor(name: Name) =
            scopes.flatMap { it.definitions.filter { it.name == name } }.lastOrNull()

        fun enterNewScope() = VariableStatus(scopes + ScopeStatus(emptySet(), emptySet()))
        fun exitScope(): VariableStatus {
            if (scopes.size <= 1) {
                return initial
            }
            val declarations = scopes.last().declarations.map(VariableDeclaration::name)
            val definitions = scopes.last().definitions.filter { it.name !in declarations }
            val updatedPrevious = scopes[scopes.size - 2].let { it.copy(definitions = it.definitions + definitions) }
            return copy(scopes = scopes.dropLast(2) + updatedPrevious)
        }

        fun exitScopeWithoutDefs(): VariableStatus {
            return copy(scopes = scopes.dropLast(1))
        }

        fun addDeclaration(declaration: VariableDeclaration): VariableStatus {
            val existingDeclaration = declarationFor(declaration.name)
            if (existingDeclaration != null) {
                throw SemanticException("Variable ${declaration.name.asString()} declared at ${declaration.declaration.span} already declared at ${existingDeclaration.declaration.span}")
            }
            return VariableStatus(
                scopes.dropLast(1) + ScopeStatus(
                    scopes.last().declarations + declaration, scopes.last().definitions
                )
            )
        }

        fun addDefinition(definition: VariableDefinition, span: Span, assignmentTree: AssignmentTree?): VariableStatus {
            val existingDeclaration = declarationFor(definition.name)
            if (existingDeclaration == null) {
                throw SemanticException("Variable ${definition.name.asString()} defined but not declared at $span")
            }
            (assignmentTree?.lValue as? LValueIdentTree)?.let { it.references = existingDeclaration.declaration }
            return VariableStatus(
                scopes.dropLast(1) + ScopeStatus(
                    scopes.last().declarations, scopes.last().definitions + definition
                )
            )
        }

        fun checkUsage(name: NameTree, span: Span): Declaration {
            val declaration = declarationFor(name.name)
            if (declaration == null) {
                throw SemanticException("Variable ${name.name.asString()} used but not defined at $span")
            }
            if (definitionFor(name.name) == null) {
                throw SemanticException("Variable ${name.name.asString()} used but not declared at $span")
            }
            return declaration.declaration
        }

        fun defineAllUndefined(): VariableStatus {
            val undefinedDefinitions =
                scopes.flatMap { it.declarations }.filter { definitionFor(it.name) == null }
                    .map { VariableDefinition(it.name) }
            return VariableStatus(
                scopes.dropLast(1) + scopes.last().copy(definitions = scopes.last().definitions + undefinedDefinitions)
            )
        }

        fun intersectLastDefinitions(afterThenStatus: VariableStatus): VariableStatus {
            return VariableStatus(
                scopes.dropLast(1) + scopes.last()
                    .copy(definitions = scopes.last().definitions.intersect(afterThenStatus.scopes.last().definitions))
            )
        }

        companion object {
            val initial = VariableStatus(listOf())
        }
    }
}
