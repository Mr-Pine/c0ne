package de.mr_pine.c0ne.backend.aasm

import de.mr_pine.c0ne.backend.Register

data class VirtualRegister(val id: Int) : Register {
    override fun toString() = "%$id"
}
