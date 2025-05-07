import edu.kit.kastel.vads.compiler.semantic.SemanticException
import org.junit.jupiter.api.assertThrows
import kotlin.test.Test
import kotlin.test.assertEquals

class SemanticTest {
    @Test
    fun `decimal literal MININT`() {
        val expected = Int.MIN_VALUE.toULong().toLong()
        val program = """
            int main() {
                int a = $expected; 
                return a;
            }
        """.trimIndent()
        val result = TestUtil.compileAndRun(program)
        assertEquals(expected.mod(256), result)
    }

    @Test
    fun `decimal literal MININT + 1`() {
        val expected = Integer.toUnsignedLong(Int.MIN_VALUE) + 1
        val program = """
            int main() {
                int a = $expected; 
                return a;
            }
        """.trimIndent()
        assertThrows<SemanticException> {
            val result = TestUtil.compileAndRun(program)
            assertEquals(expected.mod(256), result)
        }
    }

    @Test
    fun `missing return`() {
        val program = """
            int main() {
                int a = 0;
            }
        """.trimIndent()
        assertThrows<SemanticException> {
            TestUtil.compileAndRun(program)
        }
    }

    @Test
    fun `uninitialized return`() {
        val program = """
            int main() {
                int a;
                return a;
            }
        """.trimIndent()
        assertThrows<SemanticException> {
            TestUtil.compileAndRun(program)
        }
    }

    @Test
    fun `uninitialized variable`() {
        val program = """
            int main() {
                int a;
                int b = a + 3;
                return b;
            }
        """.trimIndent()
        assertThrows<SemanticException> {
            TestUtil.compileAndRun(program)
        }
    }

    @Test
    fun `undeclared variable`() {
        val program = """
            int main() {
                return a;
            }
        """.trimIndent()
        assertThrows<SemanticException> {
            TestUtil.compileAndRun(program)
        }
    }

    @Test
    fun `reinitialized variable`() {
        val program = """
            int main() {
                int a = 0;
                int a = 1;
                return a;
            }
        """.trimIndent()
        assertThrows<SemanticException> {
            TestUtil.compileAndRun(program)
        }
    }

    @Test
    fun `initialize declared variable`() {
        val expected = 42
        val program = """
            int main() {
                int a;
                int a = $expected;
                return a;
            }
        """.trimIndent()
        assertThrows<SemanticException> {
            val result = TestUtil.compileAndRun(program)
            assertEquals(expected.mod(256), result)
        }
    }

    @Test
    fun `assign declared variable`() {
        val expected = 42
        val program = """
            int main() {
                int a;
                a = $expected;
                return a;
            }
        """.trimIndent()
        val result = TestUtil.compileAndRun(program)
        assertEquals(expected.mod(256), result)
    }
}