package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.NextGenSimpleX86RegAlloc
import de.mr_pine.c0ne.backend.x86.X86Register
import de.mr_pine.c0ne.backend.x86.X86Register.RealRegister
import de.mr_pine.c0ne.ir.node.Node

sealed interface Argument {
    data class Immediate(val value: Int) : Argument {
        override val nodeValue = null

        context(alloc: NextGenSimpleX86RegAlloc)
        override fun concretize() = alloc.concretize(this)
        override fun render(size: Int) = value.toString()
    }

    sealed interface RegMem : Argument {
        sealed interface Register : RegMem {
            data class RealRegister(val register: X86Register.RealRegister) : Register {
                override val nodeValue = null

                context(alloc: NextGenSimpleX86RegAlloc)
                override fun concretize() = alloc.concretize(this)
                override fun render(size: Int) = register.render(size)
            }

            data class RegisterFor(val arg: Argument) : Register {
                override val nodeValue = arg.nodeValue

                context(alloc: NextGenSimpleX86RegAlloc)
                override fun concretize() = alloc.concretize(this)
                override fun render(size: Int) = error("Can't render abstract $this")
            }

            data class EcxOf(val from: Argument) : Register {
                override val nodeValue = from.nodeValue

                context(alloc: NextGenSimpleX86RegAlloc)
                override fun concretize() = alloc.concretize(this)
                override fun render(size: Int) = error("Can't render abstract $this")
            }
        }

        data class StackOverflowSlot(val index: Int) : RegMem {
            override val nodeValue = null

            context(alloc: NextGenSimpleX86RegAlloc)
            override fun concretize() = alloc.concretize(this)
            private fun sizePrefix(size: Int): String {
                return when (size) {
                    8 -> "QWORD"
                    4 -> "DWORD"
                    1 -> "BYTE"
                    else -> error("Unknown size $size")
                }
            }
            override fun render(size: Int) = "${sizePrefix(size)} PTR [${RealRegister.RSP} + ${index * 4}]"
        }

        data class RegMemFor(val arg: Argument) : RegMem {
            override val nodeValue = arg.nodeValue

            context(alloc: NextGenSimpleX86RegAlloc)
            override fun concretize() = alloc.concretize(this)
            override fun render(size: Int) = error("Can't render abstract $this")
        }
    }

    data class NodeValue(val node: Node) : RegMem {
        override val nodeValue = this

        context(alloc: NextGenSimpleX86RegAlloc)
        override fun concretize() = alloc.concretize(this)
        override fun render(size: Int) = error("Can't render abstract node value $this")
    }

    val nodeValue: NodeValue?

    context(alloc: NextGenSimpleX86RegAlloc)
    fun concretize(): Argument
    fun render(size: Int = 4): String
}