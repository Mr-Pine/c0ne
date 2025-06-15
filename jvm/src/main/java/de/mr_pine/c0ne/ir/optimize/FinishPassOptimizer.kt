package de.mr_pine.c0ne.ir.optimize

import de.mr_pine.c0ne.ir.IrGraph

interface FinishPassOptimizer {
    fun optimize(irGraph: IrGraph)
}