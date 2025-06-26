package de.mr_pine.c0ne

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.core.main
import com.github.ajalt.clikt.parameters.arguments.argument
import com.github.ajalt.clikt.parameters.types.path
import de.mr_pine.c0ne.backend.x86.NextGenX86CodeGenerator
import de.mr_pine.c0ne.ir.SsaTranslation
import de.mr_pine.c0ne.ir.optimize.ControlFlowPrune
import de.mr_pine.c0ne.ir.optimize.LocalValueNumbering
import de.mr_pine.c0ne.ir.optimize.MultiOptimizer
import de.mr_pine.c0ne.ir.util.YCompPrinter
import de.mr_pine.c0ne.lexer.Lexer
import de.mr_pine.c0ne.parser.ParseException
import de.mr_pine.c0ne.parser.Parser
import de.mr_pine.c0ne.parser.TokenSource
import de.mr_pine.c0ne.parser.ast.ProgramTree
import de.mr_pine.c0ne.semantic.SemanticAnalysis
import de.mr_pine.c0ne.semantic.SemanticException
import java.io.File
import java.nio.file.Path
import java.nio.file.attribute.PosixFilePermission
import kotlin.io.path.getPosixFilePermissions
import kotlin.io.path.readText
import kotlin.io.path.setPosixFilePermissions
import kotlin.io.path.writeBytes
import kotlin.system.exitProcess

class C0ne : CliktCommand() {
    val input: Path by argument(help = "Input file").path(mustExist = true, canBeDir = false)
    val output: Path by argument(help = "Output file").path(mustExist = false, canBeDir = false)


    override fun run() {
        val input = input.readText()
        val assembly = try {
            compileToAssembly(input)
        } catch (semantic: SemanticException) {
            semantic.printStackTrace()
            exitProcess(ExitCodes.SEMANTIC_ERROR.code)
        } catch (parse: ParseException) {
            parse.printStackTrace()
            exitProcess(ExitCodes.LEX_PARSE_ERROR.code)
        }
        val code = NextGenX86CodeGenerator.postprocess(assembly)
        output.writeBytes(code)
        output.setPosixFilePermissions(
            setOf(
                PosixFilePermission.OWNER_EXECUTE, PosixFilePermission.GROUP_EXECUTE,
                PosixFilePermission.OTHERS_EXECUTE
            ) + output.getPosixFilePermissions()
        )
    }

    companion object {
        fun compileToAssembly(source: String): String {
            val program = lexAndParse(source)
            SemanticAnalysis(program).analyze()
            val graphs = program.topLevelTrees.map { function ->
                val optimizer = MultiOptimizer(/*ConstantFolding(), */LocalValueNumbering())
                val finishPassOptimizer = ControlFlowPrune()
                val translation = SsaTranslation(function, optimizer, finishPassOptimizer)
                translation.translate()
            }

            val firstGraph = graphs.first()
            graphs.map {
                File("/tmp/graph-${it.name()}.vcg").writeText(YCompPrinter.print(it/*, Schedule(firstGraph)*/))
            }

            val nextGenX86CodeGenerator = NextGenX86CodeGenerator(graphs)
            return nextGenX86CodeGenerator.generateAssembly()
        }

        private fun lexAndParse(input: String): ProgramTree {
            val lexer = Lexer.forString(input)
            val tokenSource = TokenSource(lexer)
            val parser = Parser(tokenSource)
            return parser.parseProgram()
        }
    }
}

enum class ExitCodes(val code: Int) {
    LEX_PARSE_ERROR(42),
    SEMANTIC_ERROR(7)
}

fun main(args: Array<String>) = C0ne().main(args)