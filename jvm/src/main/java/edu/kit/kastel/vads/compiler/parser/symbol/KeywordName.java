package edu.kit.kastel.vads.compiler.parser.symbol;

import de.mr_pine.c0ne.lexer.KeywordType;

record KeywordName(KeywordType type) implements Name {
    @Override
    public String asString() {
        return type.getKeyword();
    }
}
