package de.mr_pine.c0ne.parser.ast

import de.mr_pine.c0ne.parser.type.Type

interface ExpressionTree : Tree {
    val type: Type
}
