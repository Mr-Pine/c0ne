package de.mr_pine.c0ne.semantic

import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.parser.ast.*
import de.mr_pine.c0ne.parser.visitor.Visitor
import edu.kit.kastel.vads.compiler.parser.symbol.Name

/** Checks that variables are
 * - declared before assignment
 * - not declared twice
 * - not initialized twice
 * - assigned before referenced */
class VariableStatusAnalysis : Visitor<VariableStatusAnalysis.VariableStatus, VariableStatusAnalysis.VariableStatus> {

    override fun visit(
        assignmentTree: AssignmentTree,
        data: VariableStatus
    ): VariableStatus {
        val status = assignmentTree.expression.accept(this, data)
        if (assignmentTree.lValue !is LValueIdentTree) {
            throw NotImplementedError("Only assignments to variables are currently supported")
        }
        return status.addDefinition(Definition(assignmentTree.lValue.name.name), assignmentTree.span)
    }

    override fun visit(
        blockTree: BlockTree,
        data: VariableStatus
    ): VariableStatus {
        var status = data.enterNewScope()
        for (statement in blockTree.statements) {
            status = statement.accept(this, status)
        }
        return status.exitScope()
    }

    override fun visit(
        binaryOperationTree: BinaryOperationTree,
        data: VariableStatus
    ): VariableStatus {
        var data = binaryOperationTree.lhs.accept(this, data)
        data = binaryOperationTree.rhs.accept(this, data)
        return data
    }

    override fun visit(
        declarationTree: DeclarationTree,
        data: VariableStatus
    ): VariableStatus {
        var status = data.addDeclaration(Declaration(declarationTree.name.name, declarationTree))
        if (declarationTree.initializer != null) {
            status = declarationTree.initializer.accept(this, status)
            status = status.addDefinition(Definition(declarationTree.name.name), declarationTree.span)
        }
        return status
    }

    override fun visit(
        functionTree: FunctionTree,
        data: VariableStatus
    ): VariableStatus {
        return functionTree.body.accept(this, data)
    }

    override fun visit(
        identExpressionTree: IdentExpressionTree,
        data: VariableStatus
    ): VariableStatus {
        data.checkUsage(identExpressionTree.name.name, identExpressionTree.span)
        return data
    }

    override fun visit(
        ternaryOperationTree: TernaryOperationTree,
        data: VariableStatus
    ): VariableStatus {
        var status = ternaryOperationTree.condition.accept(this, data)
        status = ternaryOperationTree.thenExpression.accept(this, status)
        status = ternaryOperationTree.elseExpression.accept(this, status)
        return status
    }

    override fun visit(
        literalIntTree: LiteralTree.LiteralIntTree,
        data: VariableStatus
    ): VariableStatus {
        return data
    }

    override fun visit(
        literalBoolTree: LiteralTree.LiteralBoolTree,
        data: VariableStatus
    ): VariableStatus {
        return data
    }

    override fun visit(
        lValueIdentTree: LValueIdentTree,
        data: VariableStatus
    ): VariableStatus {
        return data
    }

    override fun visit(
        nameTree: NameTree,
        data: VariableStatus
    ): VariableStatus {
        return data
    }

    override fun visit(
        unaryOperationTree: UnaryOperationTree,
        data: VariableStatus
    ): VariableStatus {
        return unaryOperationTree.accept(this, data)
    }

    override fun visit(
        programTree: ProgramTree,
        data: VariableStatus
    ): VariableStatus {
        var status = data
        for (function in programTree.topLevelTrees) {
            status = function.accept(this, status)
        }
        return status
    }

    override fun visit(
        ifTree: IfTree,
        data: VariableStatus
    ): VariableStatus {
        val status = ifTree.condition.accept(this, data).enterNewScope()
        val afterThenStatus = ifTree.thenTree.accept(this, status).exitScope()
        val afterElseStatus = ifTree.elseTree?.accept(this, status)?.exitScope()

        return afterElseStatus?.intersectLastDefinitions(afterThenStatus) ?: afterThenStatus
    }

    override fun visit(
        whileTree: WhileTree,
        data: VariableStatus
    ): VariableStatus {
        var status = whileTree.condition.accept(this, data)
        status = status.enterNewScope()
        status = whileTree.loopBody.accept(this, status)
        return status.exitScope()
    }

    override fun visit(
        forTree: ForTree,
        data: VariableStatus
    ): VariableStatus {
        var status = data.enterNewScope()
        status = forTree.initializer?.accept(this, status) ?: status
        status = forTree.condition.accept(this, status)
        status = status.enterNewScope().enterNewScope()
        status = forTree.loopBody.accept(this, status)
        status = status.exitScope()
        status = forTree.step?.accept(this, status) ?: status
        return status.exitScope().exitScope()
    }

    override fun visit(
        breakTree: BreakTree,
        data: VariableStatus
    ): VariableStatus {
        return data.defineAllUndefined()
    }

    override fun visit(
        continueTree: ContinueTree,
        data: VariableStatus
    ): VariableStatus {
        return data.defineAllUndefined()
    }

    override fun visit(
        returnTree: ReturnTree,
        data: VariableStatus
    ): VariableStatus {
        val status = returnTree.expression.accept(this, data)
        return status.defineAllUndefined()
    }

    override fun visit(
        typeTree: TypeTree,
        data: VariableStatus
    ): VariableStatus {
        return data
    }

    data class Declaration(val name: Name, val declaration: DeclarationTree)
    data class Definition(val name: Name)

    private data class ScopeStatus(
        val declarations: Set<Declaration>,
        val definitions: Set<Definition>
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
            val definitions = scopes.last().definitions
            val updatedPrevious = scopes[scopes.size - 2].let { it.copy(definitions = it.definitions + definitions) }
            return copy(scopes = scopes.dropLast(2) + updatedPrevious)
        }

        fun addDeclaration(declaration: Declaration): VariableStatus {
            val existingDeclaration = declarationFor(declaration.name)
            if (existingDeclaration != null) {
                throw SemanticException("Variable ${declaration.name.asString()} declared at ${declaration.declaration.span} already declared at ${existingDeclaration.declaration.span}")
            }
            return VariableStatus(
                scopes.dropLast(1) + ScopeStatus(
                    scopes.last().declarations + declaration,
                    scopes.last().definitions
                )
            )
        }

        fun addDefinition(definition: Definition, span: Span): VariableStatus {
            val existingDeclaration = declarationFor(definition.name)
            if (existingDeclaration == null) {
                throw SemanticException("Variable ${definition.name.asString()} defined but not declared at $span")
            }
            return VariableStatus(
                scopes.dropLast(1) + ScopeStatus(
                    scopes.last().declarations,
                    scopes.last().definitions + definition
                )
            )
        }

        fun checkUsage(name: Name, span: Span) {
            if (definitionFor(name) == null) {
                throw SemanticException("Variable ${name.asString()} used but not declared at $span")
            }
        }

        fun defineAllUndefined(): VariableStatus {
            val undefinedDefinitions =
                scopes.flatMap { it.declarations }.filter { definitionFor(it.name) == null }.map { Definition(it.name) }
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
