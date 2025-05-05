package edu.kit.kastel.vads.compiler.parser.ast;

public sealed interface StatementTree extends Tree permits AssignmentTree, BlockTree, DeclarationTree, ReturnTree {
}
