package de.mr_pine.c0ne.backend.x86.instructions

import de.mr_pine.c0ne.backend.x86.X86RegAlloc
import de.mr_pine.c0ne.ir.node.Node

sealed interface Argument {
    data class Immediate(val value: Int) : Argument {
        override val nodeValue = null

        context(alloc: X86RegAlloc)
        override fun concretize() = alloc.concretize(this)
        override fun render(size: Int) = value.toString()
    }

    sealed interface RegMem : Argument {
        sealed interface Register : RegMem {
            enum class RealRegister(val size4: String, val size1: String) : Register {
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
                R15("r15d", "r15b"), ;

                override val nodeValue = null
                override fun toString() = name.lowercase()

                context(alloc: X86RegAlloc)
                override fun concretize() = alloc.concretize(this)

                override fun render(size: Int): String {
                    return when (size) {
                        1 -> size1
                        4 -> size4
                        8 -> name.lowercase()
                        else -> error("Unknown register size")
                    }
                }

            }

            data class RegisterFor(val arg: Argument) : Register {
                override val nodeValue = arg.nodeValue

                context(alloc: X86RegAlloc)
                override fun concretize() = alloc.concretize(this)
                override fun render(size: Int) = error("Can't render abstract $this")
            }

            data class EcxOf(val from: Argument) : Register {
                override val nodeValue = from.nodeValue

                context(alloc: X86RegAlloc)
                override fun concretize() = alloc.concretize(this)
                override fun render(size: Int) = error("Can't render abstract $this")
            }
        }

        data class StackOverflowSlot(val offset: Int) : RegMem {
            override val nodeValue = null

            context(alloc: X86RegAlloc)
            override fun concretize() = alloc.concretize(this)
            private fun sizePrefix(size: Int): String {
                return when (size) {
                    8 -> "QWORD"
                    4 -> "DWORD"
                    1 -> "BYTE"
                    else -> error("Unknown size $size")
                }
            }

            override fun render(size: Int) = "${sizePrefix(size)} PTR [${Register.RealRegister.RBP} - $offset]"
        }

        data class RegMemFor(val arg: Argument) : RegMem {
            override val nodeValue = arg.nodeValue

            context(alloc: X86RegAlloc)
            override fun concretize() = alloc.concretize(this)
            override fun render(size: Int) = error("Can't render abstract $this")
        }
    }

    data class NodeValue(val node: Node) : RegMem {
        override val nodeValue = this

        context(alloc: X86RegAlloc)
        override fun concretize() = alloc.concretize(this)
        override fun render(size: Int) = error("Can't render abstract node value $this")
    }

    val nodeValue: NodeValue?

    context(alloc: X86RegAlloc)
    fun concretize(): Argument
    fun render(size: Int = 4): String
}