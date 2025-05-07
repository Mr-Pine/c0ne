package de.mr_pine.c0ne.backend.x86

import de.mr_pine.c0ne.backend.RegisterAllocator

interface X86RegisterAllocation: RegisterAllocator.RegisterAllocation<X86Register> {
    val overflowCount: Int
}