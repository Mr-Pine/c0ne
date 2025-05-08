package de.mr_pine.c0ne.backend

import edu.kit.kastel.vads.compiler.ir.node.Block
import edu.kit.kastel.vads.compiler.ir.node.ConstIntNode
import edu.kit.kastel.vads.compiler.ir.node.Node
import edu.kit.kastel.vads.compiler.ir.node.ProjNode
import edu.kit.kastel.vads.compiler.ir.node.ReturnNode
import edu.kit.kastel.vads.compiler.ir.node.StartNode


val Node.needsRegister: Boolean
    get() = !(this is ProjNode || this is StartNode || this is Block || this is ReturnNode || this is ConstIntNode)