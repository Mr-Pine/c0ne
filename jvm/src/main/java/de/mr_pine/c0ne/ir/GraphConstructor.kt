package de.mr_pine.c0ne.ir

import de.mr_pine.c0ne.ir.node.*
import de.mr_pine.c0ne.ir.optimize.Optimizer
import de.mr_pine.c0ne.parser.symbol.Name

internal class GraphConstructor(private val optimizer: Optimizer, name: String) {
    val graph: IrGraph = IrGraph(name)
    private val currentDef: MutableMap<Name, MutableMap<Block, Node>> = mutableMapOf()
    private val incompletePhis: MutableMap<Block, MutableMap<Name, Phi>> = mutableMapOf()
    private val currentSideEffect: MutableMap<Block, Node> = mutableMapOf()
    private val incompleteSideEffectPhis: MutableMap<Block, Phi> = mutableMapOf()
    private val sealedBlocks: MutableSet<Block> = mutableSetOf()
    var currentBlock = this.graph.startBlock
    private val loopBlockStack: MutableList<Block> = mutableListOf()
    private val loopFollowStack: MutableList<Block> = mutableListOf()

    init {
        // the start block never gets any more predecessors
        sealBlock(this.currentBlock)
    }

    fun newStart(): Node {
        assert(currentBlock == this.graph.startBlock) { "start must be in start block" }
        return StartNode(currentBlock)
    }

    fun newBlock(label: String): Block {
        return Block(graph, label)
    }

    fun newAdd(left: Node, right: Node): Node {
        return this.optimizer.transform(AddNode(currentBlock, left, right))
    }

    fun newSub(left: Node, right: Node): Node {
        return this.optimizer.transform(SubNode(currentBlock, left, right))
    }

    fun newMul(left: Node, right: Node): Node {
        return this.optimizer.transform(MulNode(currentBlock, left, right))
    }

    fun newDiv(left: Node, right: Node): Node {
        return this.optimizer.transform(DivNode(currentBlock, left, right, readCurrentSideEffect()))
    }

    fun newMod(left: Node, right: Node): Node {
        return this.optimizer.transform(ModNode(currentBlock, left, right, readCurrentSideEffect()))
    }

    fun newLeftShift(left: Node, right: Node): Node {
        return this.optimizer.transform(ArithmeticLeftShiftNode(currentBlock, left, right))
    }

    fun newRightShift(left: Node, right: Node): Node {
        return this.optimizer.transform(ArithmeticRightShiftNode(currentBlock, left, right))
    }

    fun newBitwiseAnd(left: Node, right: Node): Node {
        return this.optimizer.transform(BitwiseAndNode(currentBlock, left, right))
    }

    fun newXor(left: Node, right: Node): Node {
        return this.optimizer.transform(XorNode(currentBlock, left, right))
    }

    fun newBitwiseOr(left: Node, right: Node): Node {
        return this.optimizer.transform(BitwiseOrNode(currentBlock, left, right))
    }

    fun newBitwiseNot(value: Node): Node {
        return this.optimizer.transform(BitwiseNotNode(currentBlock, value))
    }

    fun newLessThan(left: Node, right: Node): Node {
        return this.optimizer.transform(LessThanNode(currentBlock, left, right))
    }

    fun newLessThanOrEqual(left: Node, right: Node): Node {
        return this.optimizer.transform(LessThanEqNode(currentBlock, left, right))
    }

    fun newGreaterThan(left: Node, right: Node): Node {
        return newLogicalNot(newLessThanOrEqual(left, right))
    }

    fun newGreaterThanOrEqual(left: Node, right: Node): Node {
        return newLogicalNot(newLessThan(left, right))
    }

    fun newEquals(left: Node, right: Node, size: Int): Node = optimizer.transform(
        EqualsNode(currentBlock, left, right, size)
    )

    fun newNotEquals(left: Node, right: Node, size: Int): Node = newLogicalNot(newEquals(left, right, size))

    fun newLogicalNot(value: Node): Node {
        return newXor(value, newConstInt(1))
    }

    fun newConstInt(value: Int): Node {
        // always move const into start block, this allows better deduplication
        // and resultingly in better value numbering
        return this.optimizer.transform(ConstIntNode(this.graph.startBlock, value))
    }

    fun newConstBool(value: Boolean): Node {
        // always move const into start block, this allows better deduplication
        // and resultingly in better value numbering
        return this.optimizer.transform(ConstBoolNode(this.graph.startBlock, value))
    }

    fun newReturn(result: Node): Node {
        return this.optimizer.transform(ReturnNode(currentBlock, readCurrentSideEffect(), result))
    }

    fun newIf(condition: Node): Node {
        return this.optimizer.transform(IfNode(currentBlock, condition))
    }

    fun newJump(): Node {
        return this.optimizer.transform(JumpNode(currentBlock))
    }

    fun newSideEffectProj(node: Node): Node {
        return this.optimizer.transform(ProjNode(currentBlock, node, ProjNode.SimpleProjectionInfo.SIDE_EFFECT))
    }

    fun newResultProj(node: Node): Node {
        return ProjNode(currentBlock, node, ProjNode.SimpleProjectionInfo.RESULT)
    }

    fun newIfTrueProjection(node: Node): ProjNode {
        return ProjNode(currentBlock, node, ProjNode.SimpleProjectionInfo.IF_TRUE)
    }

    fun newIfFalseProjection(node: Node): ProjNode {
        return ProjNode(currentBlock, node, ProjNode.SimpleProjectionInfo.IF_FALSE)
    }

    fun newPhi(block: Block): Phi {
        // don't transform phi directly, it is not ready yet
        return Phi(block)
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
            value = newPhi(block)
            this.incompletePhis.computeIfAbsent(block) { mutableMapOf() }.put(variable, value)
        } else if (block.predecessors().size == 1) {
            value = readVariable(variable, block.predecessors().first().block)
        } else {
            value = newPhi(block)
            writeVariable(variable, block, value)
            value = addPhiOperands(variable, value)
        }
        writeVariable(variable, block, value)
        return value
    }

    fun addPhiOperands(variable: Name, phi: Phi): Node {
        for (pred in phi.block.predecessors()) {
            val operand = readVariable(variable, pred.block)
            if (operand !is UndefNode) {
                phi.appendOperand(operand)
            }
        }
        return tryRemoveTrivialPhi(phi)
    }

    fun tryRemoveTrivialPhi(phi: Phi): Node {
        val other = phi.predecessors().toSet() - phi

        if (other.isEmpty()) {
            return UndefNode(phi.block)
        } else if (other.size == 1) {
            val replacement = other.first()
            for (succ in graph.successors(phi)) {
                for ((idx, _) in succ.predecessors().withIndex().filter { it.value == phi }) {
                    succ.setPredecessor(idx, replacement)
                    if (succ is Phi && succ.block in sealedBlocks) {
                        tryRemoveTrivialPhi(succ)
                    }
                }
            }
            return replacement
        }

        return phi
    }

    fun pushLoopBlock(block: Block) {
        this.loopBlockStack.add(block)
    }

    fun popLoopBlock(block: Block) {
        assert(this.loopBlockStack.removeLast() == block) { "popped block is not the last one on the stack" }
    }

    fun getLoopBlock(): Block {
        return this.loopBlockStack.last()
    }

    fun pushLoopFollow(block: Block) {
        this.loopFollowStack.add(block)
    }

    fun popLoopFollow(block: Block) {
        assert(this.loopFollowStack.removeLast() == block) { "popped block is not the last one on the stack" }
    }

    fun getLoopFollow(): Block {
        return this.loopFollowStack.last()
    }

    fun sealBlock(block: Block) {
        if (block in sealedBlocks) {
            return
        }
        for ((variable, phi) in this.incompletePhis.getOrDefault(block, mapOf()).entries) {
            val replacement = addPhiOperands(variable, phi)
            if (this.currentDef[variable]!![block] == phi) {
                this.currentDef[variable]!![block] = replacement
            }
        }
        incompletePhis.remove(block)
        this.incompleteSideEffectPhis[block]?.let { phi ->
            addPhiOperands(phi)
        }
        incompleteSideEffectPhis.remove(block)
        this.sealedBlocks.add(block)
    }

    fun writeCurrentSideEffect(node: Node) {
        writeSideEffect(currentBlock, node)
    }

    private fun writeSideEffect(block: Block, node: Node) {
        this.currentSideEffect.put(block, node)
    }

    fun readCurrentSideEffect(): Node {
        return readSideEffect(currentBlock)
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
            value = newPhi(block)
            val old = this.incompleteSideEffectPhis.put(block, value)
            assert(old == null) { "double readSideEffectRecursive for $block" }
        } else if (block.predecessors().size == 1) {
            value = readSideEffect(block.predecessors().first().block)
        } else {
            value = newPhi(block)
            writeSideEffect(block, value)
            value = addPhiOperands(value)
        }
        writeSideEffect(block, value)
        return value
    }

    fun addPhiOperands(phi: Phi): Node {
        for (pred in phi.block.predecessors()) {
            val operand = readSideEffect(pred.block)
            if (operand !is UndefNode) {
                phi.appendOperand(operand)
            }
        }
        return tryRemoveTrivialPhi(phi)
    }
}
