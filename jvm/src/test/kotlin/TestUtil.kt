import de.mr_pine.c0ne.backend.x86.X86CodeGenerator
import edu.kit.kastel.vads.compiler.ir.SsaTranslation
import edu.kit.kastel.vads.compiler.ir.optimize.LocalValueNumbering
import de.mr_pine.c0ne.lexer.Lexer
import de.mr_pine.c0ne.parser.Parser
import de.mr_pine.c0ne.parser.TokenSource
import de.mr_pine.c0ne.parser.ast.ProgramTree
import de.mr_pine.c0ne.semantic.SemanticAnalysis
import java.nio.file.attribute.PosixFilePermission
import java.nio.file.attribute.PosixFilePermissions
import kotlin.io.path.*

class TestUtil {
    companion object {
        fun compileAndRun(input: String): Int {
            val program = lexAndParse(input)
            SemanticAnalysis(program).analyze()
            val graphs = program.topLevelTrees.map { function ->
                val translation = SsaTranslation(function, LocalValueNumbering())
                translation.translate()
            }

            val code = X86CodeGenerator().generateCode(graphs)
            val output = createTempFile(
                "c0ne_test_output", "", PosixFilePermissions.asFileAttribute(
                    setOf(
                        PosixFilePermission.OWNER_EXECUTE,
                        PosixFilePermission.OWNER_WRITE, PosixFilePermission.OWNER_READ
                    )
                )
            )
            output.writeBytes(code)
            output.setPosixFilePermissions(
                setOf(
                    PosixFilePermission.OWNER_EXECUTE, PosixFilePermission.GROUP_EXECUTE,
                    PosixFilePermission.OTHERS_EXECUTE
                ) + output.getPosixFilePermissions()
            )

            return ProcessBuilder(output.absolutePathString()).start().waitFor()
        }

        private fun lexAndParse(input: String): ProgramTree {
            val lexer = Lexer.forString(input)
            val tokenSource = TokenSource(lexer)
            val parser = Parser(tokenSource)
            return parser.parseProgram()
        }
    }
}