package de.mr_pine.c0ne.backend

import de.mr_pine.c0ne.ir.node.*


val Node.needsRegister: Boolean
    get() = !((this is ProjNode && projectionInfo() !is ProjNode.NamedParameterProjectionInfo) || this is StartNode || this is Block || this is ExitNode || this is ConstIntNode || this is ConstBoolNode)