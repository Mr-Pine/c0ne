import crow.SamplesProvider
import de.mr_pine.c0ne.C0ne
import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.BeforeAll
import org.junit.jupiter.api.parallel.Execution
import org.junit.jupiter.api.parallel.ExecutionMode
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.MethodSource
import java.io.ByteArrayOutputStream
import java.io.IOException
import java.util.concurrent.TimeUnit
import kotlin.test.assertEquals

internal class SamplesTest {

    companion object {

        const val TIMEOUT = 1
        val timeoutUnit = TimeUnit.MINUTES
        const val CURRENT_LAB = "Lab 3"
        val crow_client_command = arrayOf("nix", "run", "-j", "8", "github:I-Al-Istannen/crow?ref=v0.1.12#client", "--")

        @JvmStatic
        fun samples(): List<SamplesProvider.SampleInfo> {
            return SamplesProvider.provideTests(CURRENT_LAB);
        }

        @BeforeAll
        @JvmStatic
        @Throws(IOException::class, InterruptedException::class)
        fun buildCompiler() {
            val result = execute(
                arrayOf(
                    "./gradlew",
                    "installDist"
                ), 5, TimeUnit.MINUTES
            )
            assertEquals(0, result.exitCode, "building compiler failed")
        }


        private fun execute(args: Array<String>, timeout: Int, unit: TimeUnit?): ExecutionResult {
            val procRef = Reference<Process>()

            val shutdownHook = Thread { kill(procRef) }
            Runtime.getRuntime().addShutdownHook(shutdownHook)

            val process = ProcessBuilder(*args)
                .start()

            val out = ByteArrayOutputStream()
            process.inputStream.transferTo(out)
            process.errorStream.transferTo(out)

            procRef.value = process

            val exited = process.waitFor(timeout.toLong(), unit)
            val result = ExecutionResult(exited, if (exited) process.exitValue() else -1, out.toString())

            process.destroyForcibly()
            out.close()
            Runtime.getRuntime().removeShutdownHook(shutdownHook)

            return result
        }

        private fun kill(processReference: Reference<Process>) {
            if (processReference.value != null) {
                processReference.value!!.destroyForcibly()
            }
        }
    }

    @ParameterizedTest
    @MethodSource("samples")
    @Execution(ExecutionMode.CONCURRENT)
    fun assertCompilationResultCorrect(sample: SamplesProvider.SampleInfo) {
        val result = execute(
            crow_client_command +
                    arrayOf(
                        "run-test",
                        "--test-dir", sample.file.parent.toString(),
                        "--test-id", sample.file.fileName.toString().replace(".crow-test.md", ""),
                        "--compiler-run", "./run.sh"

                    ), TIMEOUT, timeoutUnit
        )

        Assertions.assertTrue(result.terminated, "tester did not terminate")
        Assertions.assertEquals(0, result.exitCode, "tester failed:" + System.lineSeparator() + result.output)
    }

    @ParameterizedTest
    @MethodSource("samples")
    fun assertCompilationDeterministic(sample: SamplesProvider.SampleInfo) {
        println("Compiling for initial result")
        val result = runCatching { C0ne.compileToAssembly(sample.program) }

        for (i in 1..25) {
            println("Compiling subsequent result number $i")

            val compile = { C0ne.compileToAssembly(sample.program) }

            if (result.exceptionOrNull() != null) {
                Assertions.assertThrows(
                    result.exceptionOrNull()!!.javaClass,
                    { compile() },
                    "subsequent compilation did not result in error"
                )
            } else {
                assertEquals(result.getOrThrow(), compile(), "subsequent result does not match initial")
            }
        }
    }

    private data class ExecutionResult(val terminated: Boolean, val exitCode: Int, val output: String)

    private data class Reference<T>(var value: T? = null)
}