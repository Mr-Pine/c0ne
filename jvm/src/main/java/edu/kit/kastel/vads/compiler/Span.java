package edu.kit.kastel.vads.compiler;

public sealed interface Span {
    Position start();
    Position end();

    Span merge(Span later);

    record SimpleSpan(Position start, Position end) implements Span {
        @Override
        public Span merge(Span later) {
            return new SimpleSpan(start(), later.end());
        }

        @Override
        public String toString() {
            return "[" + start() + "|" + end() + "]";
        }
    }
}
