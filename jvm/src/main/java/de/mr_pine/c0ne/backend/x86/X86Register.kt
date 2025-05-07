package de.mr_pine.c0ne.backend.x86

import de.mr_pine.c0ne.backend.Register

sealed interface X86Register : Register {
    enum class RealRegister(val availableForAllocation: Boolean) : X86Register {
        EAX(false),
        EBX(true),
        ECX(true),
        EDX(false),
        ESI(true),
        EDI(true),
        RSP(false),
        RBP(false),
        R8D(true),
        R9D(true),
        R10D(true),
        R11D(true),
        R12D(true),
        R13D(true),
        R14D(true),
        R15D(false);

        override fun toString() = name.lowercase()
    }

    data class OverflowSlot(val index: Int) : X86Register {
        override fun toString() = "DWORD PTR [${RealRegister.RSP} + $index]"
    }
}