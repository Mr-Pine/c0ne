package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc
import de.mr_pine.c0ne.backend.x86.X86Register
import de.mr_pine.c0ne.ir.node.Node

sealed interface Argument {
    data class Immediate(val value: Int) : Argument {
        override val nodeValue = null

        context(alloc: NextGenSimpleX86RegAlloc)
        override fun concretize() = alloc.concretize(this)
    }

    sealed interface RegMem : Argument {
        sealed interface Register : RegMem {
            data class RealRegister(val register: X86Register) : Register {
                override val nodeValue = null

                context(alloc: NextGenSimpleX86RegAlloc)
                override fun concretize() = alloc.concretize(this)
            }

            data class RegisterFor(val arg: Argument) : Register {
                override val nodeValue = arg.nodeValue

                context(alloc: NextGenSimpleX86RegAlloc)
                override fun concretize() = alloc.concretize(this)
            }

            data class EcxOf(val from: Argument) : Register {
                override val nodeValue = from.nodeValue

                context(alloc: NextGenSimpleX86RegAlloc)
                override fun concretize() = alloc.concretize(this)
            }
        }

        data class StackOverflowSlot(val index: Int) : RegMem {
            override val nodeValue = null

            context(alloc: NextGenSimpleX86RegAlloc)
            override fun concretize() = alloc.concretize(this)
        }

        data class RegMemFor(val arg: Argument) : RegMem {
            override val nodeValue = arg.nodeValue

            context(alloc: NextGenSimpleX86RegAlloc)
            override fun concretize() = alloc.concretize(this)
        }
    }

    data class NodeValue(val node: Node) : RegMem {
        override val nodeValue = this

        context(alloc: NextGenSimpleX86RegAlloc)
        override fun concretize() = alloc.concretize(this)
    }

    val nodeValue: NodeValue?

    context(alloc: NextGenSimpleX86RegAlloc)
    fun concretize(): Argument
}