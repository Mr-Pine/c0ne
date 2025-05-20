package edu.kit.kastel.vads.compiler.parser.visitor;

import de.mr_pine.c0ne.parser.ast.AssignmentTree;
import de.mr_pine.c0ne.parser.ast.BinaryOperationTree;
import de.mr_pine.c0ne.parser.ast.BlockTree;
import de.mr_pine.c0ne.parser.ast.DeclarationTree;
import de.mr_pine.c0ne.parser.ast.FunctionTree;
import de.mr_pine.c0ne.parser.ast.IdentExpressionTree;
import de.mr_pine.c0ne.parser.ast.LValueIdentTree;
import de.mr_pine.c0ne.parser.ast.LiteralTree;
import de.mr_pine.c0ne.parser.ast.NameTree;
import de.mr_pine.c0ne.parser.ast.UnaryOperationTree;
import de.mr_pine.c0ne.parser.ast.ProgramTree;
import de.mr_pine.c0ne.parser.ast.ReturnTree;
import de.mr_pine.c0ne.parser.ast.TypeTree;

public interface Visitor<T, R> {

    R visit(AssignmentTree assignmentTree, T data);

    R visit(BinaryOperationTree binaryOperationTree, T data);

    R visit(BlockTree blockTree, T data);

    R visit(DeclarationTree declarationTree, T data);

    R visit(FunctionTree functionTree, T data);

    R visit(IdentExpressionTree identExpressionTree, T data);

    R visit(LiteralTree.LiteralIntTree literalIntTree, T data);

    R visit(LiteralTree.LiteralBoolTree literalBoolTree, T data);

    R visit(LValueIdentTree lValueIdentTree, T data);

    R visit(NameTree nameTree, T data);

    R visit(UnaryOperationTree unaryOperationTree, T data);

    R visit(ProgramTree programTree, T data);

    R visit(ReturnTree returnTree, T data);

    R visit(TypeTree typeTree, T data);
}
