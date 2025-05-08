package de.mr_pine.c0ne.backend.x86

import de.mr_pine.c0ne.backend.Register

sealed interface X86Register : Register {
    enum class RealRegister : X86Register {
        EAX,
        EBX,
        ECX,
        EDX,
        ESI,
        EDI,
        RSP,
        RBP,
        R8D,
        R9D,
        R10D,
        R11D,
        R12D,
        R13D,
        R14D,
        R15D;

        override fun toString() = name.lowercase()
    }

    data class OverflowSlot(val index: Int) : X86Register {
        override fun toString() = "DWORD PTR [${RealRegister.RSP} + ${index * 4}]"
    }
}