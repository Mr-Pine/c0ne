package de.mr_pine.c0ne.parser.ast

interface Declaration : Tree {
    val typeDeclaration: TypeTree
    val name: NameTree

    val type
        get() = typeDeclaration.type
}
