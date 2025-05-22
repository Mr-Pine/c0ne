package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import edu.kit.kastel.vads.compiler.parser.symbol.Name
import de.mr_pine.c0ne.parser.visitor.Visitor

@JvmRecord
data class NameTree(@JvmField val name: Name, override val span: Span) : Tree {
    override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
        return visitor.visit(this, data)
    }
}
