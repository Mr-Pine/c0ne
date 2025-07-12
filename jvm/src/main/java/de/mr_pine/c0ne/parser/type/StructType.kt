package de.mr_pine.c0ne.parser.type

import de.mr_pine.c0ne.parser.ast.StructureTree
import de.mr_pine.c0ne.parser.symbol.Name

data class StructType(val name: Name) : Type {
    override fun asString() = "struct ${name.asString()}"

    var references: StructureTree? = null
    val fieldTypes
        get() = references!!.fields.associate { it.name.name to it.type }
}