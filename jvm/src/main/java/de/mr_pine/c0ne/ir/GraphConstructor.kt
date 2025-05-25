package de.mr_pine.c0ne.ir

import de.mr_pine.c0ne.ir.node.AddNode
import de.mr_pine.c0ne.ir.node.Block
import de.mr_pine.c0ne.ir.node.ConstIntNode
import de.mr_pine.c0ne.ir.node.DivNode
import de.mr_pine.c0ne.ir.node.ModNode
import de.mr_pine.c0ne.ir.node.MulNode
import de.mr_pine.c0ne.ir.node.Node
import de.mr_pine.c0ne.ir.node.Phi
import de.mr_pine.c0ne.ir.node.ProjNode
import de.mr_pine.c0ne.ir.node.ReturnNode
import de.mr_pine.c0ne.ir.node.StartNode
import de.mr_pine.c0ne.ir.node.SubNode
import de.mr_pine.c0ne.ir.optimize.Optimizer
import de.mr_pine.c0ne.parser.symbol.Name

internal class GraphConstructor(private val optimizer: Optimizer, name: String) {
    private val graph: IrGraph = IrGraph(name)
    private val currentDef: MutableMap<Name, MutableMap<Block, Node>> = mutableMapOf()
    private val incompletePhis: MutableMap<Block, MutableMap<Name, Phi>> = mutableMapOf()
    private val currentSideEffect: MutableMap<Block, Node> = mutableMapOf()
    private val incompleteSideEffectPhis: MutableMap<Block, Phi> = mutableMapOf()
    private val sealedBlocks: MutableSet<Block> = mutableSetOf()
    private val currentBlock = this.graph.startBlock()

    init {
        // the start block never gets any more predecessors
        sealBlock(this.currentBlock)
    }

    fun newStart(): Node {
        assert(currentBlock() == this.graph.startBlock()) { "start must be in start block" }
        return StartNode(currentBlock())
    }

    fun newAdd(left: Node, right: Node): Node {
        return this.optimizer.transform(AddNode(currentBlock(), left, right))
    }

    fun newSub(left: Node, right: Node): Node {
        return this.optimizer.transform(SubNode(currentBlock(), left, right))
    }

    fun newMul(left: Node, right: Node): Node {
        return this.optimizer.transform(MulNode(currentBlock(), left, right))
    }

    fun newDiv(left: Node, right: Node): Node {
        return this.optimizer.transform(DivNode(currentBlock(), left, right, readCurrentSideEffect()))
    }

    fun newMod(left: Node, right: Node): Node {
        return this.optimizer.transform(ModNode(currentBlock(), left, right, readCurrentSideEffect()))
    }

    fun newReturn(result: Node): Node {
        return ReturnNode(currentBlock(), readCurrentSideEffect(), result)
    }

    fun newConstInt(value: Int): Node {
        // always move const into start block, this allows better deduplication
        // and resultingly in better value numbering
        return this.optimizer.transform(ConstIntNode(this.graph.startBlock(), value))
    }

    fun newSideEffectProj(node: Node): Node {
        return ProjNode(currentBlock(), node, ProjNode.SimpleProjectionInfo.SIDE_EFFECT)
    }

    fun newResultProj(node: Node): Node {
        return ProjNode(currentBlock(), node, ProjNode.SimpleProjectionInfo.RESULT)
    }

    fun currentBlock(): Block {
        return this.currentBlock
    }

    fun newPhi(): Phi {
        // don't transform phi directly, it is not ready yet
        return Phi(currentBlock())
    }

    fun graph(): IrGraph {
        return this.graph
    }

    fun writeVariable(variable: Name, block: Block, value: Node) {
        this.currentDef.computeIfAbsent(variable) { mutableMapOf() }.put(block, value)
    }

    fun readVariable(variable: Name, block: Block): Node {
        val node = this.currentDef.getOrDefault(
            variable, mutableMapOf()
        )[block]
        if (node != null) {
            return node
        }
        return readVariableRecursive(variable, block)
    }


    private fun readVariableRecursive(variable: Name, block: Block): Node {
        var value: Node
        if (!this.sealedBlocks.contains(block)) {
            value = newPhi()
            this.incompletePhis.computeIfAbsent(block) { mutableMapOf() }.put(variable, value)
        } else if (block.predecessors().size == 1) {
            value = readVariable(variable, block.predecessors().first().block())
        } else {
            value = newPhi()
            writeVariable(variable, block, value)
            value = addPhiOperands(variable, value)
        }
        writeVariable(variable, block, value)
        return value
    }

    fun addPhiOperands(variable: Name, phi: Phi): Node {
        for (pred in phi.block().predecessors()) {
            phi.appendOperand(readVariable(variable, pred.block()))
        }
        return tryRemoveTrivialPhi(phi)
    }

    fun tryRemoveTrivialPhi(phi: Phi): Node {
        // TODO: the paper shows how to remove trivial phis.
        // as this is not a problem in Lab 1 and it is just
        // a simplification, we recommend to implement this
        // part yourself.
        return phi
    }

    fun sealBlock(block: Block) {
        for (entry in this.incompletePhis.getOrDefault(block, mapOf()).entries) {
            addPhiOperands(entry.key, entry.value)
        }
        this.sealedBlocks.add(block)
    }

    fun writeCurrentSideEffect(node: Node) {
        writeSideEffect(currentBlock(), node)
    }

    private fun writeSideEffect(block: Block, node: Node) {
        this.currentSideEffect.put(block, node)
    }

    fun readCurrentSideEffect(): Node {
        return readSideEffect(currentBlock())
    }

    private fun readSideEffect(block: Block): Node {
        val node = this.currentSideEffect[block]
        if (node != null) {
            return node
        }
        return readSideEffectRecursive(block)
    }

    private fun readSideEffectRecursive(block: Block): Node {
        var value: Node
        if (!this.sealedBlocks.contains(block)) {
            value = newPhi()
            val old = this.incompleteSideEffectPhis.put(block, value)
            assert(old == null) { "double readSideEffectRecursive for $block" }
        } else if (block.predecessors().size == 1) {
            value = readSideEffect(block.predecessors().first().block())
        } else {
            value = newPhi()
            writeSideEffect(block, value)
            value = addPhiOperands(value)
        }
        writeSideEffect(block, value)
        return value
    }

    fun addPhiOperands(phi: Phi): Node {
        for (pred in phi.block().predecessors()) {
            phi.appendOperand(readSideEffect(pred.block()))
        }
        return tryRemoveTrivialPhi(phi)
    }
}
