package de.mr_pine.c0ne.parser.visitor

import de.mr_pine.c0ne.parser.ast.*
import de.mr_pine.c0ne.parser.ast.LiteralTree.LiteralBoolTree
import de.mr_pine.c0ne.parser.ast.LiteralTree.LiteralIntTree

/** A visitor that traverses a tree in postorder
 *
 * @param <T> a type for additional data
 * @param <R> a type for a return type
</R></T> */
open class RecursivePostorderVisitor<T, R>(private val visitor: Visitor<T, R>) : Visitor<T, R> {
    override fun visit(assignmentTree: AssignmentTree, data: T): R {
        var r = assignmentTree.lValue.accept<T, R>(this, data)
        r = assignmentTree.expression.accept<T, R>(this, accumulate(data, r))
        r = this.visitor.visit(assignmentTree, accumulate(data, r))
        return r
    }

    override fun visit(binaryOperationTree: BinaryOperationTree, data: T): R {
        var r = binaryOperationTree.lhs.accept<T, R>(this, data)
        r = binaryOperationTree.rhs.accept<T, R>(this, accumulate(data, r))
        r = this.visitor.visit(binaryOperationTree, accumulate(data, r))
        return r
    }

    override fun visit(blockTree: BlockTree, data: T): R {
        var d = data
        for (statement in blockTree.statements) {
            val r = statement.accept(this, d)
            d = accumulate(d, r)
        }
        return this.visitor.visit(blockTree, d)
    }

    override fun visit(declarationTree: DeclarationTree, data: T): R {
        var r = declarationTree.typeDeclaration.accept(this, data)
        r = declarationTree.name.accept(this, accumulate(data, r))
        if (declarationTree.initializer != null) {
            r = declarationTree.initializer.accept(this, accumulate(data, r))
        }
        r = this.visitor.visit(declarationTree, accumulate(data, r))
        return r
    }

    override fun visit(functionTree: FunctionTree, data: T): R {
        var r = functionTree.returnType.accept(this, data)
        r = functionTree.name.accept(this, accumulate(data, r))
        r = functionTree.body.accept(this, accumulate(data, r))
        r = this.visitor.visit(functionTree, accumulate(data, r))
        return r
    }

    override fun visit(ternaryOperationTree: TernaryOperationTree, data: T): R {
        var r = ternaryOperationTree.condition.accept(this, data)
        r = ternaryOperationTree.thenExpression.accept(this, accumulate(data, r))
        r = ternaryOperationTree.elseExpression.accept(this, accumulate(data, r))
        return this.visitor.visit(ternaryOperationTree, accumulate(data, r))
    }

    override fun visit(identExpressionTree: IdentExpressionTree, data: T): R {
        var r = identExpressionTree.name.accept(this, data)
        r = this.visitor.visit(identExpressionTree, accumulate(data, r))
        return r
    }

    override fun visit(literalIntTree: LiteralIntTree, data: T): R {
        return this.visitor.visit(literalIntTree, data)
    }

    override fun visit(literalBoolTree: LiteralBoolTree, data: T): R {
        return this.visitor.visit(literalBoolTree, data)
    }

    override fun visit(lValueIdentTree: LValueIdentTree, data: T): R {
        var r = lValueIdentTree.name.accept(this, data)
        r = this.visitor.visit(lValueIdentTree, accumulate(data, r))
        return r
    }

    override fun visit(nameTree: NameTree, data: T): R {
        return this.visitor.visit(nameTree, data)
    }

    override fun visit(unaryOperationTree: UnaryOperationTree, data: T): R {
        var r = unaryOperationTree.expression.accept(this, data)
        r = this.visitor.visit(unaryOperationTree, accumulate(data, r))
        return r
    }

    override fun visit(programTree: ProgramTree, data: T): R {
        var d = data
        for (tree in programTree.topLevelTrees) {
            val r = tree.accept(this, d)
            d = accumulate(data, r)
        }
        return this.visitor.visit(programTree, d)
    }

    override fun visit(ifTree: IfTree, data: T): R {
        var r = ifTree.condition.accept(this, data)
        r = ifTree.thenTree.accept(this, accumulate(data, r))
        if (ifTree.elseTree != null) {
            r = ifTree.thenTree.accept(this, accumulate(data, r))
        }
        r = this.visitor.visit(ifTree, accumulate(data, r))
        return r
    }

    override fun visit(whileTree: WhileTree, data: T): R {
        var r = whileTree.condition.accept(this, data)
        r = whileTree.loopBody.accept(this, accumulate(data, r))
        r = this.visitor.visit(whileTree, accumulate(data, r))
        return r
    }

    override fun visit(forTree: ForTree, data: T): R {
        var r: R
        if (forTree.initializer != null) {
            r = forTree.initializer.accept(this, data)
            r = forTree.condition.accept(this, accumulate(data, r))
        } else {
            r = forTree.condition.accept(this, data)
        }
        if (forTree.step != null) {
            r = forTree.step.accept(this, accumulate(data, r))
        }
        r = forTree.loopBody.accept(this, accumulate(data, r))
        r = this.visitor.visit(forTree, accumulate(data, r))
        return r
    }

    override fun visit(breakTree: BreakTree, data: T): R {
        return this.visitor.visit(breakTree, data)
    }

    override fun visit(continueTree: ContinueTree, data: T): R {
        return this.visitor.visit(continueTree, data)
    }

    override fun visit(returnTree: ReturnTree, data: T): R {
        var r = returnTree.expression.accept(this, data)
        r = this.visitor.visit(returnTree, accumulate(data, r))
        return r
    }

    override fun visit(typeTree: TypeTree, data: T): R {
        return this.visitor.visit(typeTree, data)
    }

    protected fun accumulate(data: T, value: R): T {
        return data
    }
}
