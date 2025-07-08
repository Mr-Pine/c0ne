package de.mr_pine.c0ne.parser.type

import de.mr_pine.c0ne.parser.symbol.Name

data class StructType(val name: Name): Type {
    override fun asString() = "struct ${name.asString()}"

    override val smallSize
        get() = error("Structs have no fixed size")
}