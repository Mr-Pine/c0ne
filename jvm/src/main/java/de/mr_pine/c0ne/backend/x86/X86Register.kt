package de.mr_pine.c0ne.backend.x86

import de.mr_pine.c0ne.backend.Register

sealed interface X86Register : Register {

    enum class RealRegister(val size4: String, val size1: String) : X86Register {
        RAX("eax", "al"),
        RBX("ebx", "bl"),
        RCX("ecx", "cl"),
        RDX("edx", "dl"),
        RSI("esi", "sil"),
        RDI("edi", "dil"),
        RSP("esp", "spl"),
        RBP("ebp", "bpl"),
        R8("r8d", "r8b"),
        R9("r9d", "r9b"),
        R10("r10d", "r10b"),
        R11("r11d", "r11b"),
        R12("r12d", "r12b"),
        R13("r13d", "r13b"),
        R14("r14d", "r14b"),
        R15("r15d", "r15b"),;

        override fun toString() = name.lowercase()
        fun render(size: Int): String {
            return when (size) {
                1 -> size1
                4 -> size4
                8 -> name.lowercase()
                else -> error("Unknown register size")
            }
        }

    }

    data class OverflowSlot(val index: Int) : X86Register {
        override fun toString() = "DWORD PTR [${RealRegister.RSP} + ${index * 4}]"
    }
}