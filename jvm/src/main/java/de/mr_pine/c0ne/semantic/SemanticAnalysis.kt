package de.mr_pine.c0ne.semantic

import de.mr_pine.c0ne.parser.ast.ProgramTree
import de.mr_pine.c0ne.parser.visitor.RecursivePostorderVisitor
import de.mr_pine.c0ne.semantic.ReturnAnalysis.ReturnState

class SemanticAnalysis(private val program: ProgramTree) {
    fun analyze() {
        this.program.accept(
            RecursivePostorderVisitor(
                IntegerLiteralRangeAnalysis()
            ), Unit
        )
        this.program.accept(
            VariableStatusAnalysis(), VariableStatusAnalysis.VariableStatus.initial
        )
        this.program.accept(
            RecursivePostorderVisitor(TypeCheckAnalysis()), mutableListOf()
        )
        this.program.accept(RecursivePostorderVisitor(BreakContinueAnalysis()), mutableListOf())
        this.program.accept(
            RecursivePostorderVisitor(ReturnAnalysis()), ReturnState()
        )
        this.program.accept(
            RecursivePostorderVisitor(MainMethodAnalysis()), Unit
        )
    }
}
