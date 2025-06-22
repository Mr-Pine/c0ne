package de.mr_pine.c0ne.parser

import de.mr_pine.c0ne.parser.ast.AssignmentTree
import de.mr_pine.c0ne.parser.ast.BinaryOperationTree
import de.mr_pine.c0ne.parser.ast.BlockTree
import de.mr_pine.c0ne.parser.ast.DeclarationTree
import de.mr_pine.c0ne.parser.ast.DeclaredFunctionTree
import de.mr_pine.c0ne.parser.ast.IdentExpressionTree
import de.mr_pine.c0ne.parser.ast.LValueIdentTree
import de.mr_pine.c0ne.parser.ast.LiteralTree
import de.mr_pine.c0ne.parser.ast.NameTree
import de.mr_pine.c0ne.parser.ast.ProgramTree
import de.mr_pine.c0ne.parser.ast.ReturnTree
import de.mr_pine.c0ne.parser.ast.Tree
import de.mr_pine.c0ne.parser.ast.TypeTree
import de.mr_pine.c0ne.parser.ast.UnaryOperationTree


/** This is a utility class to help with debugging the parser. */
class Printer(private val ast: Tree) {
    private val builder = StringBuilder()
    private var requiresIndent = false
    private var indentDepth = 0

    private fun printRoot() {
        printTree(this.ast)
    }

    private fun printTree(tree: Tree) {
        when (tree) {
            is BlockTree -> {
                print("{")
                lineBreak()
                this.indentDepth++
                for (statement in tree.statements) {
                    printTree(statement)
                }
                this.indentDepth--
                print("}")
            }

            is DeclaredFunctionTree -> {
                printTree(tree.returnTypeTree)
                space()
                printTree(tree.name)
                print("(")
                printTree(tree.parameters)
                print(")")
                space()
                printTree(tree.body)
            }

            is NameTree -> print(tree.name.asString())
            is ProgramTree -> {
                for (function in tree.topLevelTrees) {
                    printTree(function)
                    lineBreak()
                }
            }

            is TypeTree -> print(tree.type.asString())
            is BinaryOperationTree -> {
                print("(")
                printTree(tree.lhs)
                print(")")
                space()
                this.builder.append(tree.operatorType)
                space()
                print("(")
                printTree(tree.rhs)
                print(")")
            }

            is LiteralTree.LiteralIntTree -> this.builder.append(tree.value)
            is UnaryOperationTree -> {
                this.builder.append(tree.operator.type)
                print("(")
                printTree(tree.expression)
                print(")")
            }

            is AssignmentTree -> {
                printTree(tree.lValue)
                space()
                this.builder.append(tree.operator)
                space()
                printTree(tree.expression)
                semicolon()
            }

            is DeclarationTree -> {
                printTree(tree.typeDeclaration)
                space()
                printTree(tree.name)
                if (tree.initializer != null) {
                    print(" = ")
                    printTree(tree.initializer)
                }
                semicolon()
            }

            is ReturnTree -> {
                print("return ")
                printTree(tree.expression)
                semicolon()
            }

            is LValueIdentTree -> printTree(tree.name)
            is IdentExpressionTree -> printTree(tree.name)
            else -> throw IllegalStateException("Unexpected value: $tree")
        }
    }

    private fun print(str: String?) {
        if (this.requiresIndent) {
            this.requiresIndent = false
            this.builder.append(" ".repeat(4 * this.indentDepth))
        }
        this.builder.append(str)
    }

    private fun lineBreak() {
        this.builder.append("\n")
        this.requiresIndent = true
    }

    private fun semicolon() {
        this.builder.append(";")
        lineBreak()
    }

    private fun space() {
        this.builder.append(" ")
    }

    companion object {
        fun print(ast: Tree): String {
            val printer = Printer(ast)
            printer.printRoot()
            return printer.builder.toString()
        }
    }
}
