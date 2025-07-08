package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.Span
import de.mr_pine.c0ne.parser.symbol.Name
import de.mr_pine.c0ne.parser.type.Type
import de.mr_pine.c0ne.parser.visitor.Visitor

sealed interface FunctionTree: Tree {
    val name: Name
    val returnType: Type
    val parameterTypes: List<Type>

    data class BuiltinFunction(override val name: Name, override val returnType: Type, override val parameterTypes: List<Type>): FunctionTree {
        override val span = Span.Synthetic

        override fun <T, R> accept(visitor: Visitor<T, R>, data: T): R {
            return visitor.visit(this, data)
        }
    }
}