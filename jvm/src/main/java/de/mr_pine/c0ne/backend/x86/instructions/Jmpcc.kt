package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc

sealed class Jmpcc(condition: String, val target: Label) : Instruction("J$condition ${target.mnemonic}") {
    context(alloc: X86RegAlloc)
    override fun concretize() = this
}


class Je(target: Label) : Jmpcc("E", target)
class Jne(target: Label) : Jmpcc("NE", target)
class Jg(target: Label) : Jmpcc("G", target)
class Jge(target: Label) : Jmpcc("GE", target)
class Jl(target: Label) : Jmpcc("L", target)
class Jle(target: Label) : Jmpcc("LE", target)
