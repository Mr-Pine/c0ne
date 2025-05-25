package de.mr_pine.c0ne

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.core.main
import com.github.ajalt.clikt.parameters.arguments.argument
import com.github.ajalt.clikt.parameters.types.path
import de.mr_pine.c0ne.backend.x86.X86CodeGenerator
import de.mr_pine.c0ne.ir.optimize.ConstantFolding
import de.mr_pine.c0ne.ir.optimize.MultiOptimizer
import edu.kit.kastel.vads.compiler.ir.SsaTranslation
import de.mr_pine.c0ne.ir.optimize.LocalValueNumbering
import de.mr_pine.c0ne.lexer.Lexer
import de.mr_pine.c0ne.parser.ParseException
import de.mr_pine.c0ne.parser.Parser
import de.mr_pine.c0ne.parser.TokenSource
import de.mr_pine.c0ne.semantic.SemanticAnalysis
import de.mr_pine.c0ne.semantic.SemanticException
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
        val program = lexAndParse(input)
        try {
            SemanticAnalysis(program).analyze()
        } catch (e: SemanticException) {
            e.printStackTrace()
            exitProcess(ExitCodes.SEMANTIC_ERROR.code)
        }
        val graphs = program.topLevelTrees.map { function ->
            val optimizer = MultiOptimizer(ConstantFolding(), LocalValueNumbering())
            val translation = SsaTranslation(function, optimizer)
            translation.translate()
        }

        val code = X86CodeGenerator().generateCode(graphs)
        output.writeBytes(code)
        output.setPosixFilePermissions(
            setOf(
                PosixFilePermission.OWNER_EXECUTE, PosixFilePermission.GROUP_EXECUTE,
                PosixFilePermission.OTHERS_EXECUTE
            ) + output.getPosixFilePermissions()
        )
    }

    companion object {
        private fun lexAndParse(input: Path) = try {
            val lexer = Lexer.forString(input.readText())
            val tokenSource = TokenSource(lexer)
            val parser = Parser(tokenSource)
            parser.parseProgram()
        } catch (e: ParseException) {
            e.printStackTrace()
            exitProcess(ExitCodes.LEX_PARSE_ERROR.code)
        }
    }
}

enum class ExitCodes(val code: Int) {
    LEX_PARSE_ERROR(42),
    SEMANTIC_ERROR(7)
}

fun main(args: Array<String>) = C0ne().main(args)