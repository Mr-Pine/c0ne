package de.mr_pine.c0ne.semantic

import de.mr_pine.c0ne.parser.ast.ProgramTree
import de.mr_pine.c0ne.parser.ast.StructureTree
import de.mr_pine.c0ne.parser.type.StructType
import de.mr_pine.c0ne.parser.visitor.NoOpVisitor

class RecursiveStructAnalysis : NoOpVisitor<MutableMap<StructureTree, Set<StructureTree>>> {
    override fun visit(
        structureTree: StructureTree,
        data: MutableMap<StructureTree, Set<StructureTree>>
    ) {
        data[structureTree] = structureTree.fields.mapNotNull { (it.type as? StructType)?.references }.toSet()
    }

    override fun visit(
        programTree: ProgramTree,
        data: MutableMap<StructureTree, Set<StructureTree>>
    ) {
        var current: Map<StructureTree, Set<StructureTree>> = mapOf()
        var next: Map<StructureTree, Set<StructureTree>> = data
        while (current != next) {
            current = next
            next = current.mapValues { (k, v) -> v + v.flatMap { current[it]!! }.toSet() }
        }

        for ((struct, references) in next) {
            if (struct in references) {
                throw SemanticException("Self referential struct $struct")
            }
        }
    }
}