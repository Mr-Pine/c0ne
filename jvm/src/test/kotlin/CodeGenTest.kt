import kotlin.test.Test
import kotlin.test.assertEquals

class CodeGenTest {

    @Test
    fun `fibonacci 100`() {
        val size = 100
        val expected = run {
            var f1 = 0
            var f2 = 1
            for (i in 3..size) {
                val tmp = f2
                f2 = f1 + f2
                f1 = tmp
            }
            f2
        }
        val program = buildString {
            appendLine("int main() {")
            appendLine("int var1 = 0;")
            appendLine("int var2 = 1;")
            for (i in 3..size) {
                appendLine("int var$i = var${i - 2} + var${i - 1};")
            }
            appendLine("return 1 / (var$size - (-889489150));")
            appendLine("}")
        }
        val result = TestUtil.compileAndRun(program)
        assertEquals(expected.mod(136), result)
    }

    @Test
    fun `fancy div by 0`() {
        val expected = 3
        val program = """
            int main() {
                int a = 3;
                int b = a % a;
                int c = a / a;
                int d = b - c;
                int e = 0x8000000 * 0x10;
                return e / d;
            }
        """.trimIndent()
        val result = TestUtil.compileAndRun(program)
        assertEquals(136, result)
    }

}