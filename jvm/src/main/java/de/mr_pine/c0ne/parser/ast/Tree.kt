package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import edu.kit.kastel.vads.compiler.parser.visitor.Visitor

interface Tree {
    val span: Span

    fun <T, R> accept(visitor: Visitor<T, R>, data: T): R
}
