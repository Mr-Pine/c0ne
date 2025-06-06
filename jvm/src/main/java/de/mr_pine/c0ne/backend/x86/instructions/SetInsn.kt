package de.mr_pine.c0ne.backend.x86.instructions

abstract class SetInsn(condition: String, val target: Argument.RegMem) : Instruction("SET$condition", target) {
    override fun render(): String {
        return "$mnemonic ${target.render(1)}"
    }
}
