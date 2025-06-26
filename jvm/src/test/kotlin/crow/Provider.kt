package crow

import org.commonmark.node.*
import org.commonmark.parser.Parser
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path

object SamplesProvider {
    private val PARSER: Parser = Parser.builder().build()

    data class SampleInfo(val file: Path, val program: String, val limitedToCategory: Boolean = false)

    fun provideTests(validCategory: String): List<SampleInfo> {
        val testsDir = Path.of("../crow-tests/")
        return Files.walk(testsDir).filter(Files::isRegularFile)
            .filter { it.fileName.toString().endsWith(".crow-test.md") }.toList().map(::parseTest)
            .filter { sample: SampleInfo ->
                !sample.limitedToCategory || sample.file.parent.fileName.toString() == validCategory
            }
    }

    private fun parseTest(file: Path): SampleInfo {
        val content: String?
        try {
            content = Files.readString(file)
        } catch (e: IOException) {
            throw IllegalStateException("Cannot read file content", e)
        }
        val node: Node = PARSER.parse(content)
        val programParser = ProgramParser()
        node.accept(programParser)

        return SampleInfo(file, programParser.program ?: error("No program found"), programParser.limitedToCategory)
    }

    private class ProgramParser : AbstractVisitor() {
        private var lastHeading: String? = null

        var program: String? = null
            private set
        var limitedToCategory = false
            private set


        override fun visit(text: Text) {
            if (text.parent is Heading) {
                lastHeading = text.literal
            }

            super.visit(text)
        }

        override fun visit(fencedCodeBlock: FencedCodeBlock) {
            when (lastHeading!!.trim { it <= ' ' }) {
                "ProgramArgumentFile" -> program = fencedCodeBlock.literal
                "Limited to Category" -> limitedToCategory = fencedCodeBlock.literal.trim { it <= ' ' }.toBoolean()
            }

            super.visit(fencedCodeBlock)
        }
    }
}