package de.mr_pine.c0ne.parser.symbol

@JvmRecord
internal data class IdentName(val identifier: String) : Name {
    override fun asString(): String {
        return this.identifier
    }
}
