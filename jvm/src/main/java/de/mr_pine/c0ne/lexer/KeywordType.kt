package de.mr_pine.c0ne.lexer

enum class KeywordType(val keyword: String) {
    STRUCT("struct"),
    IF("if"),
    ELSE("else"),
    WHILE("while"),
    FOR("for"),
    CONTINUE("continue"),
    BREAK("break"),
    RETURN("return"),
    ASSERT("assert"),
    TRUE("true"),
    FALSE("false"),
    NULL("NULL"),
    PRINT("print"),
    READ("read"),
    ALLOC("alloc"),
    ALLOC_ARRAY("alloc_array"),
    INT("int"),
    BOOL("bool"),
    VOID("void"),
    CHAR("char"),
    STRING("string"),
    ;

    override fun toString(): String {
        return keyword
    }
}
