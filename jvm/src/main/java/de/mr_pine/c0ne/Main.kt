package de.mr_pine.c0ne

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.core.main
import com.github.ajalt.clikt.parameters.arguments.argument
import com.github.ajalt.clikt.parameters.types.path
import de.mr_pine.c0ne.backend.aasm.AasmCodeGenerator
import de.mr_pine.c0ne.backend.x86.X86CodeGenerator
import edu.kit.kastel.vads.compiler.ir.SsaTranslation
import edu.kit.kastel.vads.compiler.ir.optimize.LocalValueNumbering
import edu.kit.kastel.vads.compiler.lexer.Lexer
import edu.kit.kastel.vads.compiler.parser.ParseException
import edu.kit.kastel.vads.compiler.parser.Parser
import edu.kit.kastel.vads.compiler.parser.TokenSource
import edu.kit.kastel.vads.compiler.semantic.SemanticAnalysis
import edu.kit.kastel.vads.compiler.semantic.SemanticException
import java.nio.file.Path
import java.nio.file.attribute.PosixFilePermission
import kotlin.io.path.getPosixFilePermissions
import kotlin.io.path.readText
import kotlin.io.path.setPosixFilePermissions
import kotlin.io.path.writeBytes
import kotlin.io.path.writeText
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
            val translation = SsaTranslation(function, LocalValueNumbering())
            translation.translate()
        }

        // TODO: generate assembly and invoke gcc instead of generating abstract assembly
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