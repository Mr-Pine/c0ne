package de.mr_pine.c0ne.semantic

import de.mr_pine.c0ne.parser.ast.AssignmentTree
import de.mr_pine.c0ne.parser.ast.DeclarationTree
import de.mr_pine.c0ne.parser.ast.IdentExpressionTree
import de.mr_pine.c0ne.parser.ast.LValueIdentTree
import de.mr_pine.c0ne.parser.ast.NameTree
import de.mr_pine.c0ne.semantic.VariableStatusAnalysis.VariableStatus
import edu.kit.kastel.vads.compiler.parser.visitor.NoOpVisitor
import edu.kit.kastel.vads.compiler.parser.visitor.Unit
import edu.kit.kastel.vads.compiler.semantic.Namespace
import java.util.*
import java.util.function.BinaryOperator

/** Checks that variables are
 * - declared before assignment
 * - not declared twice
 * - not initialized twice
 * - assigned before referenced */
class VariableStatusAnalysis : NoOpVisitor<Namespace<VariableStatus?>> {
    override fun visit(assignmentTree: AssignmentTree, data: Namespace<VariableStatus?>): Unit? {
        val lValue = assignmentTree.lValue
        when (lValue) {
            is LValueIdentTree -> {
                val status = data.get(lValue.name)
                if (assignmentTree.operator.type.isSelfAssignOperator) {
                    checkInitialized(lValue.name, status)
                } else {
                    checkDeclared(lValue.name, status)
                }
                data.put(
                    lValue.name,
                    VariableStatus.INITIALIZED
                ) { _, replacement -> replacement }
            }

            else -> throw IllegalStateException("Unexpected value: " + assignmentTree.lValue)
        }
        return super.visit(assignmentTree, data)
    }

    override fun visit(declarationTree: DeclarationTree, data: Namespace<VariableStatus?>): Unit? {
        val status = if (declarationTree.initializer == null)
            VariableStatus.DECLARED
        else
            VariableStatus.INITIALIZED
        data.put(
            declarationTree.name,
            status
        ) { existing: VariableStatus?, replacement: VariableStatus? ->
            throw SemanticException("variable is already $existing. Cannot be $replacement here.")
        }
        return super.visit(declarationTree, data)
    }

    override fun visit(identExpressionTree: IdentExpressionTree, data: Namespace<VariableStatus?>): Unit? {
        val status = data.get(identExpressionTree.name)
        checkInitialized(identExpressionTree.name, status)
        return super.visit(identExpressionTree, data)
    }

    enum class VariableStatus {
        DECLARED,
        INITIALIZED;

        override fun toString(): String {
            return name.lowercase(Locale.ROOT)
        }
    }

    companion object {
        private fun checkDeclared(name: NameTree?, status: VariableStatus?) {
            if (status == null) {
                throw SemanticException("Variable $name must be declared before assignment")
            }
        }

        private fun checkInitialized(name: NameTree?, status: VariableStatus?) {
            if (status != VariableStatus.INITIALIZED) {
                throw SemanticException("Variable $name must be initialized before usage")
            }
        }
    }
}
