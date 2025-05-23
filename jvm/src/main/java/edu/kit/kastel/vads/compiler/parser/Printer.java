package edu.kit.kastel.vads.compiler.parser;

import de.mr_pine.c0ne.parser.ast.AssignmentTree;
import de.mr_pine.c0ne.parser.ast.BinaryOperationTree;
import de.mr_pine.c0ne.parser.ast.BlockTree;
import de.mr_pine.c0ne.parser.ast.IdentExpressionTree;
import de.mr_pine.c0ne.parser.ast.LValueIdentTree;
import de.mr_pine.c0ne.parser.ast.LiteralTree;
import de.mr_pine.c0ne.parser.ast.NameTree;
import de.mr_pine.c0ne.parser.ast.UnaryOperationTree;
import de.mr_pine.c0ne.parser.ast.ReturnTree;
import de.mr_pine.c0ne.parser.ast.Tree;
import de.mr_pine.c0ne.parser.ast.DeclarationTree;
import de.mr_pine.c0ne.parser.ast.FunctionTree;
import de.mr_pine.c0ne.parser.ast.ProgramTree;
import de.mr_pine.c0ne.parser.ast.StatementTree;
import de.mr_pine.c0ne.parser.ast.TypeTree;

/// This is a utility class to help with debugging the parser.
public class Printer {

    private final Tree ast;
    private final StringBuilder builder = new StringBuilder();
    private boolean requiresIndent;
    private int indentDepth;

    public Printer(Tree ast) {
        this.ast = ast;
    }

    public static String print(Tree ast) {
        Printer printer = new Printer(ast);
        printer.printRoot();
        return printer.builder.toString();
    }

    private void printRoot() {
        printTree(this.ast);
    }

    private void printTree(Tree tree) {
        switch (tree) {
            case BlockTree blockTree -> {
                print("{");
                lineBreak();
                this.indentDepth++;
                for (StatementTree statement : blockTree.getStatements()) {
                    printTree(statement);
                }
                this.indentDepth--;
                print("}");
            }
            case FunctionTree functionTree -> {
                printTree(functionTree.returnType);
                space();
                printTree(functionTree.name);
                print("()");
                space();
                printTree(functionTree.body);
            }
            case NameTree nameTree -> print(nameTree.getName().asString());
            case ProgramTree programTree -> {
                for (FunctionTree function : programTree.getTopLevelTrees()) {
                    printTree(function);
                    lineBreak();
                }
            }
            case TypeTree typeTree -> print(typeTree.type().asString());
            case BinaryOperationTree binaryOperationTree -> {
                print("(");
                printTree(binaryOperationTree.getLhs());
                print(")");
                space();
                this.builder.append(binaryOperationTree.getOperatorType());
                space();
                print("(");
                printTree(binaryOperationTree.getRhs());
                print(")");
            }
            case LiteralTree.LiteralIntTree literalIntTree -> this.builder.append(literalIntTree.getValue());
            case UnaryOperationTree unaryOperationTree -> {
                print("-(");
                printTree(unaryOperationTree.getExpression());
                print(")");
            }
            case AssignmentTree assignmentTree -> {
                printTree(assignmentTree.lValue);
                space();
                this.builder.append(assignmentTree.operator);
                space();
                printTree(assignmentTree.expression);
                semicolon();
            }
            case DeclarationTree declarationTree -> {
                printTree(declarationTree.getTypeDeclaration());
                space();
                printTree(declarationTree.getName());
                if (declarationTree.getInitializer() != null) {
                    print(" = ");
                    printTree(declarationTree.getInitializer());
                }
                semicolon();
            }
            case ReturnTree returnTree -> {
                print("return ");
                printTree(returnTree.expression);
                semicolon();
            }
            case LValueIdentTree lValueIdentTree -> printTree(lValueIdentTree.name);
            case IdentExpressionTree identExpressionTree -> printTree(identExpressionTree.getName());
            default -> throw new IllegalStateException("Unexpected value: " + tree);
        }
    }

    private void print(String str) {
        if (this.requiresIndent) {
            this.requiresIndent = false;
            this.builder.append(" ".repeat(4 * this.indentDepth));
        }
        this.builder.append(str);
    }

    private void lineBreak() {
        this.builder.append("\n");
        this.requiresIndent = true;
    }

    private void semicolon() {
        this.builder.append(";");
        lineBreak();
    }

    private void space() {
        this.builder.append(" ");
    }

}
