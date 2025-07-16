package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.parser.symbol.Name
import de.mr_pine.c0ne.parser.visitor.Visitor
import kotlin.math.floor

data class StructureTree(
    val nameTree: NameTree,
    val fields: List<DeclarationTree>,
    override val span: Span,
) : TopLevelTree {

    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }

    private fun padTo(size: Int, alignment: Int) = floor((size - 1) / alignment.toFloat()).toInt() * alignment + alignment


    val offsets by lazy {
        buildMap {
            var currentOffset = 0
            for (field in fields) {
                val fieldSize = field.type.size
                val alignment = field.type.alignment
                currentOffset = padTo(currentOffset, alignment)
                put(field.name.name, currentOffset)
                currentOffset += fieldSize
            }
        }
    }
    val alignment
        get() = fields.maxOfOrNull { it.type.alignment } ?: 8
    val size: Int
        get() = if (fields.isNotEmpty()) padTo(offsets.values.max() + fields.last().type.size, alignment) else 0
}
