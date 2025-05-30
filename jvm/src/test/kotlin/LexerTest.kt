import de.mr_pine.c0ne.parser.ParseException
import org.junit.jupiter.api.assertThrows
import kotlin.test.Test
import kotlin.test.assertEquals

class LexerTest {
    @Test
    fun `lexer exception test`() {
        val program = """
            int main() {
                return `:(`;
            }
        """.trimIndent()
        assertThrows<ParseException> {
            val result = TestUtil.compileAndRun(program)
        }
    }

    @Test
    fun `numeric operators`() {
        val values = listOf(1024, 12948, 12, 9807, 123, 234, 3)
        val program = """
            int main() {
                int a = 0x${values[0].toHexString()};
                int B = ${values[1]};
                int c_ = ${values[2]};
                int d0 = 0x${values[3].toHexString().uppercase()};
                int e = ${values[4]};
                int f = ${values[5]};
                return ((a + B) / c_ % d0 - e) * f;
            }
        """.trimIndent()
        val result = TestUtil.compileAndRun(program)
        val expected = ((values[0] + values[1]) / values[2] % values[3] - values[4]) * values[5]
        assertEquals(expected % 256, result)
    }

    @Test
    fun `assignment operator`() {
        val values = listOf(6, 7)
        val program = """
            int main() {
                int a = 0x${values[0].toHexString()};
                a *= ${values[1]};
                return a;
            }
        """.trimIndent()
        val result = TestUtil.compileAndRun(program)
        val expected = values[0] * values[1]
        assertEquals(expected % 256, result)
    }

    @Test
    fun `whitespace test`() {
        val expected = 42;
        val program = """
            int main() {
            // Single line comment 1
                // Single line comment 2
                /* Multiline comment
                    oh wow!
                */
                
                // /* /* Mismatched multiline in single line */
                return $expected;
            }
        """.trimIndent()
        val result = TestUtil.compileAndRun(program)
        assertEquals(expected % 256, result)
    }

    @Test
    fun `nested multiline comment`() {
        val expected = 42;
        val program = """
            int main() {
                /* Multiline comment
                    /*
                        Nested! /* even deeper */
                    */
                */
                return $expected;
            }
        """.trimIndent()
        val result = TestUtil.compileAndRun(program)
        assertEquals(expected % 256, result)
    }

    @Test
    fun `invalid nested multiline comment`() {
        val expected = 42;
        val program = """
            int main() {
                /* Multiline comment
                    /*
                        Nested! /* I'm about to forget closing this nested comment
                    */
                */
                return $expected;
            }
        """.trimIndent()
        assertThrows<ParseException> {
            val result = TestUtil.compileAndRun(program)
            assertEquals(expected % 256, result)
        }
    }

    @Test
    fun `weird multiline comment`() {
        val expected = 42;
        val program = """
            int main() {
                return $expected;
            }
            /*/
        """.trimIndent()
        assertThrows<ParseException> {
            val result = TestUtil.compileAndRun(program)
            assertEquals(expected % 256, result)
        }
    }

    @Test
    fun `broken hex literal`() {
        val program = """
            int main() {
                return 0x;
            }
        """.trimIndent()
        assertThrows<ParseException> {
            TestUtil.compileAndRun(program)
        }
    }

    @Test
    fun `no octal numbers`() {
        val program = """
            int main() {
                return 03;
            }
        """.trimIndent()
        assertThrows<ParseException> {
            TestUtil.compileAndRun(program)
        }
    }
}