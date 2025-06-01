package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc

abstract class Cmov(condition: String, val target: Argument.RegMem, val source: Argument) : Instruction("CMOV$condition", target, source) {
}
