package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc
import jdk.internal.net.http.common.TimeSource.source

class Cmp(val target: Argument.RegMem, val source: Argument) : Instruction("CMP", target, source) {
    context(alloc: NextGenSimpleX86RegAlloc)
    override fun concretize(): Instruction {
        return Cmp(target.concretize() as Argument.RegMem, source.concretize())
    }
}
