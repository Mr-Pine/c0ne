package edu.kit.kastel.vads.compiler.parser.symbol;

import de.mr_pine.c0ne.lexer.Identifier;
import de.mr_pine.c0ne.lexer.Keyword;

public sealed interface Name permits IdentName, KeywordName {

    static Name forKeyword(Keyword keyword) {
        return new KeywordName(keyword.type);
    }

    static Name forIdentifier(Identifier identifier) {
        return new IdentName(identifier.value);
    }

    String asString();
}
