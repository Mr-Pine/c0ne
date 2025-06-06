package de.mr_pine.c0ne.backend

import de.mr_pine.c0ne.ir.node.Block
import de.mr_pine.c0ne.ir.node.ConstIntNode
import de.mr_pine.c0ne.ir.node.ExitNode
import de.mr_pine.c0ne.ir.node.Node
import de.mr_pine.c0ne.ir.node.ProjNode
import de.mr_pine.c0ne.ir.node.ReturnNode
import de.mr_pine.c0ne.ir.node.StartNode


val Node.needsRegister: Boolean
    get() = !(this is ProjNode || this is StartNode || this is Block || this is ExitNode /*|| this is ConstIntNode*/)