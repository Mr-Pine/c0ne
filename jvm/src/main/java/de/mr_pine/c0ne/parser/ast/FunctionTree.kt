package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.parser.type.Type
import de.mr_pine.c0ne.parser.visitor.Visitor

sealed interface FunctionTree: Tree {
    val returnType: Type

    data class BuiltinFunction(val name: String, override val returnType: Type): FunctionTree {
        override val span = Span.Synthetic

        override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
            return visitor.visit(this, data)
        }
    }
}