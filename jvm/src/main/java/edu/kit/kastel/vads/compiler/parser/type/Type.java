package edu.kit.kastel.vads.compiler.parser.type;

public sealed interface Type permits BasicType {
    String asString();
}
