import de.mr_pine.c0ne.parser.ParseException
import org.junit.jupiter.api.assertThrows
import kotlin.test.Test
import kotlin.test.assertEquals

class ParserTest {
    @Test
    fun `program trailing stuff`() {
        val expected = 42
        val program = """
            int main() {
                return $expected;
            }
            keys
            /
        """.trimIndent()
        assertThrows<ParseException> {
            val result = TestUtil.compileAndRun(program)
            assertEquals(expected % 256, result)
        }
    }

    @Test
    fun `multiple functions`() {
        val expected = 42
        val program = """
            int main() {
                return $expected;
            }
            
            int lol() {
                return $expected;
            }
        """.trimIndent()
        assertThrows<ParseException> {
            val result = TestUtil.compileAndRun(program)
            assertEquals(expected % 256, result)
        }
    }

    @Test
    fun `empty program`() {
        val expected = 42
        val program = """
        """.trimIndent()
        assertThrows<ParseException> {
            val result = TestUtil.compileAndRun(program)
            assertEquals(expected % 256, result)
        }
    }

    @Test
    fun `lonely expression`() {
        val expected = 42
        val program = """
            int main() {
                int a = 3;
                a + 5;
                return $expected;
            }
        """.trimIndent()
        assertThrows<ParseException> {
            val result = TestUtil.compileAndRun(program)
            assertEquals(expected % 256, result)
        }
    }

    @Test
    fun `some lonely numbers`() {
        val expected = 42
        val program = """
            int main() {
                a 3 5;
                return $expected;
            }
        """.trimIndent()
        assertThrows<ParseException> {
            val result = TestUtil.compileAndRun(program)
            assertEquals(expected % 256, result)
        }
    }

    @Test
    fun `missing semicolon`() {
        val expected = 42
        val program = """
            int main() {
                int a = 3
                return $expected;
            }
        """.trimIndent()
        assertThrows<ParseException> {
            val result = TestUtil.compileAndRun(program)
            assertEquals(expected % 256, result)
        }
    }

    @Test
    fun `parenthesized lvalue`() {
        val expected = 8
        val program = """
            int main() {
                int a = 0;
                int b = 4;
                (((((a))))) = b;
                return a + b;
            }
        """.trimIndent()
        val result = TestUtil.compileAndRun(program)
        assertEquals(expected % 256, result)
    }

    @Test
    fun `mismatched parenthesized lvalue`() {
        val expected = 8
        val program = """
            int main() {
                int a = 0;
                int b = 4;
                (((((a)))) = b;
                return a + b;
            }
        """.trimIndent()
        assertThrows<ParseException> {
            val result = TestUtil.compileAndRun(program)
            assertEquals(expected % 256, result)
        }
    }

    @Test
    fun multiplication() {
        val expected = -48
        val program = """
            int main() {
                int a = 3;
                int b = 4;
                int c = 4;
                return a * b * -c;
            }
        """.trimIndent()
        val result = TestUtil.compileAndRun(program)
        assertEquals(expected.mod(256), result)
    }

    @Test
    fun `assignment chain`() {
        val expected = -48
        val program = """
            int main() {
                int a = 3;
                int b = 4;
                int c = 4;
                a = b = c; 
                return a * b * -c;
            }
        """.trimIndent()
        assertThrows<ParseException> {
            val result = TestUtil.compileAndRun(program)
            assertEquals(expected.mod(256), result)
        }
    }
}