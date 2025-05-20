package edu.kit.kastel.vads.compiler.parser;

import de.mr_pine.c0ne.lexer.Identifier;
import de.mr_pine.c0ne.lexer.Keyword;
import edu.kit.kastel.vads.compiler.lexer.KeywordType;
import edu.kit.kastel.vads.compiler.lexer.Lexer;
import de.mr_pine.c0ne.lexer.Operator;
import de.mr_pine.c0ne.lexer.Operator.OperatorType;
import de.mr_pine.c0ne.lexer.Separator;
import de.mr_pine.c0ne.lexer.Separator.SeparatorType;
import de.mr_pine.c0ne.lexer.Token;

import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

public class TokenSource {
    private final List<Token> tokens;
    private int idx;

    public TokenSource(Lexer lexer) {
        this.tokens = Stream.generate(lexer::nextToken)
            .takeWhile(Optional::isPresent)
            .map(Optional::orElseThrow)
            .toList();
    }

    TokenSource(List<Token> tokens) {
        this.tokens = List.copyOf(tokens);
    }

    public Token peek() {
        expectHasMore();
        return this.tokens.get(this.idx);
    }

    public Keyword expectKeyword(KeywordType type) {
        Token token = peek();
        if (!(token instanceof Keyword kw) || kw.type != type) {
            throw new ParseException("expected keyword '" + type + "' but got " + token);
        }
        this.idx++;
        return kw;
    }

    public Separator expectSeparator(SeparatorType type) {
        Token token = peek();
        if (!(token instanceof Separator sep) || sep.type != type) {
            throw new ParseException("expected separator '" + type + "' but got " + token);
        }
        this.idx++;
        return sep;
    }

    public Operator expectOperator(OperatorType type) {
        Token token = peek();
        if (!(token instanceof Operator op) || op.type != type) {
            throw new ParseException("expected operator '" + type + "' but got " + token);
        }
        this.idx++;
        return op;
    }
    public Identifier expectIdentifier() {
        Token token = peek();
        if (!(token instanceof Identifier ident)) {
            throw new ParseException("expected identifier but got " + token);
        }
        this.idx++;
        return ident;
    }

    public Token consume() {
        Token token = peek();
        this.idx++;
        return token;
    }

    public boolean hasMore() {
        return this.idx < this.tokens.size();
    }

    private void expectHasMore() {
        if (!hasMore()) {
            throw new ParseException("reached end of file");
        }
    }
}
