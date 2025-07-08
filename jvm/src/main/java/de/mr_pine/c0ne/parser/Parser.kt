package de.mr_pine.c0ne.parser

import de.mr_pine.c0ne.lexer.*
import de.mr_pine.c0ne.lexer.Separator.SeparatorType
import de.mr_pine.c0ne.parser.ast.*
import de.mr_pine.c0ne.parser.symbol.IdentName
import de.mr_pine.c0ne.parser.symbol.Name
import de.mr_pine.c0ne.parser.type.ArrayType
import de.mr_pine.c0ne.parser.type.BasicType
import de.mr_pine.c0ne.parser.type.PointerType
import de.mr_pine.c0ne.parser.type.StructType
import kotlin.collections.listOf

class Parser(private val tokenSource: TokenSource) {
    fun parseProgram(): ProgramTree {
        return ProgramTree(parseTopLevelElements())
    }

    private fun parseTopLevelElements() = buildList {
        while (tokenSource.hasMore()) {
            val element = if (tokenSource.peekAs<Keyword>()?.type == KeywordType.STRUCT) {
                parseStructure()
            } else {
                parseFunction()
            }
            add(element)
        }
    }

    private fun parseFunction(): DeclaredFunctionTree {
        val type = parseType()
        val identifier = this.tokenSource.expectIdentifier()
        val parameterList = parseParameterList()
        val body = parseBlock()
        return DeclaredFunctionTree(type, name(identifier), parameterList, body)
    }

    private fun parseStructure(): StructureTree {
        val structKeyword = this.tokenSource.expectKeyword(KeywordType.STRUCT)
        val identifier = this.tokenSource.expectIdentifier()

        this.tokenSource.expectSeparator(SeparatorType.BRACE_OPEN)

        val fields = buildList {
            while (tokenSource.peekAs<Separator>()?.type != SeparatorType.BRACE_CLOSE) {
                val declaration = parseDeclaration()
                if (declaration.initializer != null) {
                    throw ParseException("struct fields cannot have initializers")
                }
                tokenSource.expectSeparator(SeparatorType.SEMICOLON)
                add(declaration)
            }
        }

        this.tokenSource.expectSeparator(SeparatorType.BRACE_CLOSE)
        val finishingSemicolon = this.tokenSource.expectSeparator(SeparatorType.SEMICOLON)

        return StructureTree(name(identifier), fields, structKeyword.span.merge(finishingSemicolon.span))
    }

    private fun <T : Tree> parseParenthesizedList(elementParser: Parser.() -> T): ParenthesizedListTree<T> {
        val startSpan = tokenSource.expectSeparator(SeparatorType.PAREN_OPEN).span

        val elements = buildList {
            if (tokenSource.peekAs<Separator>()?.type != SeparatorType.PAREN_CLOSE) while (true) {
                add(elementParser())

                if (tokenSource.peekAs<Separator>()?.type == SeparatorType.PAREN_CLOSE) {
                    break
                }

                tokenSource.expectSeparator(SeparatorType.COMMA)
            }
        }

        val endSpan = tokenSource.expectSeparator(SeparatorType.PAREN_CLOSE).span

        return ParenthesizedListTree(elements, startSpan merge endSpan)
    }

    private fun parseArgumentList(): ParenthesizedListTree<ExpressionTree> =
        parseParenthesizedList { parseExpression() }

    private fun parseParameterList(): ParenthesizedListTree<ParameterTree> =
        parseParenthesizedList {
            val type = parseType()
            val name = name(tokenSource.expectIdentifier())
            ParameterTree(type, name)
        }

    private fun parseBlock(): BlockTree {
        val bodyOpen = this.tokenSource.expectSeparator(SeparatorType.BRACE_OPEN)
        val statements = buildList {
            while (this@Parser.tokenSource.peekAs<Separator>()?.let { it.type == SeparatorType.BRACE_CLOSE } != true) {
                add(parseStatement())
            }
        }
        val bodyClose = this.tokenSource.expectSeparator(SeparatorType.BRACE_CLOSE)
        return BlockTree(statements, bodyOpen.span.merge(bodyClose.span))
    }

    val Token.isType
        get() = TYPE_KEYWORDS.any { isKeyword(it) }
    val Token.isControl
        get() = CONTROL_KEYWORDS.any { isKeyword(it) }

    private fun parseStatement(): StatementTree {
        val nextToken = this.tokenSource.peek()
        val statement = if (nextToken.isType) {
            parseDeclaration()
        } else if (nextToken.isControl) {
            return parseControlStatement()
        } else if (nextToken.isSeparator(SeparatorType.BRACE_OPEN)) {
            return parseBlock()
        } else {
            parseSimple()
        }
        this.tokenSource.expectSeparator(SeparatorType.SEMICOLON)
        return statement
    }

    private fun parseDeclaration(): DeclarationTree {
        val type = parseType()

        val ident = this.tokenSource.expectIdentifier()
        val init = if (this.tokenSource.peek().isOperator(Operator.OperatorType.ASSIGN)) {
            this.tokenSource.expectOperator(Operator.OperatorType.ASSIGN)
            parseExpression()
        } else {
            null
        }
        return DeclarationTree(type, name(ident), init)
    }

    private fun parseType(): TypeTree {
        val typeKeyword = this.tokenSource.expectAnyKeyword(TYPE_KEYWORDS)
        var span = typeKeyword.span
        var type = when (typeKeyword.type) {
            KeywordType.INT -> BasicType.Integer
            KeywordType.BOOL -> BasicType.Boolean
            KeywordType.STRUCT -> {
                val name = tokenSource.expectIdentifier()
                span = span merge name.span
                StructType(Name.forIdentifier(name))
            }

            else -> throw ParseException("expected type but got $typeKeyword")
        }

        while (this.tokenSource.peekAs<Operator>()?.type == Operator.OperatorType.MUL || tokenSource.peekAs<Separator>()?.type == SeparatorType.BRACKET_OPEN) {
            val token = this.tokenSource.consume()
            when (token) {
                is Operator if token.type == Operator.OperatorType.MUL -> {
                    span = span merge token.span
                    type = PointerType(type)
                }

                is Separator if token.type == SeparatorType.BRACKET_OPEN -> {
                    span = span merge token.span
                    tokenSource.expectSeparator(SeparatorType.BRACKET_CLOSE)
                    type = ArrayType(type)
                }

                else -> throw ParseException("expected * or [ but got $token")
            }
        }

        return TypeTree(type, span)
    }

    private fun parseSimple(): StatementTree {
        // Special case print, read, flush
        if (this.tokenSource.peekAs<Keyword>()
                ?.let { it.type in listOf(KeywordType.PRINT, KeywordType.READ, KeywordType.FLUSH) } == true
        ) {
            val keyword = this.tokenSource.consume() as Keyword
            val arguments = parseArgumentList()
            return CallTree(NameTree(IdentName(keyword.type.name.lowercase()), keyword.span), arguments)
        }

        val lValue = parseLValue()
        if (this.tokenSource.peekAs<Separator>()?.type == SeparatorType.PAREN_OPEN) {
            val arguments = parseArgumentList()
            return CallTree((lValue as LValueIdentTree).name, arguments)
        } else {
            val assignmentOperator = parseAssignmentOperator()
            val expression = parseExpression()
            return AssignmentTree(lValue, assignmentOperator, expression)
        }
    }

    private fun parseAssignmentOperator(): Operator {
        val operator = tokenSource.peekAs<Operator>()
        if (operator != null) {
            return when (operator.type) {
                Operator.OperatorType.ASSIGN, Operator.OperatorType.ASSIGN_DIV, Operator.OperatorType.ASSIGN_MINUS, Operator.OperatorType.ASSIGN_MOD, Operator.OperatorType.ASSIGN_MUL, Operator.OperatorType.ASSIGN_PLUS, Operator.OperatorType.ASSIGN_OR, Operator.OperatorType.ASSIGN_AND, Operator.OperatorType.ASSIGN_XOR, Operator.OperatorType.ASSIGN_LEFT_SHIFT, Operator.OperatorType.ASSIGN_RIGHT_SHIFT -> {
                    this.tokenSource.consume()
                    operator
                }

                else -> throw ParseException("expected assignment but got " + operator.type)
            }
        }
        throw ParseException("expected assignment but got " + this.tokenSource.peek())
    }

    private fun parseLValue(): LValueTree {
        if (this.tokenSource.peek().isSeparator(SeparatorType.PAREN_OPEN)) {
            this.tokenSource.expectSeparator(SeparatorType.PAREN_OPEN)
            val inner = parseLValue()
            this.tokenSource.expectSeparator(SeparatorType.PAREN_CLOSE)
            return inner
        }
        val identifier = this.tokenSource.expectIdentifier()
        return LValueIdentTree(name(identifier))
    }

    private fun parseControlStatement(): ControlTree {
        val nextToken = this.tokenSource.peekAs<Keyword>()!!
        return when (nextToken.type) {
            KeywordType.IF -> {
                val keyword = tokenSource.expectKeyword(KeywordType.IF)
                tokenSource.expectSeparator(SeparatorType.PAREN_OPEN)
                val condition = parseExpression()
                tokenSource.expectSeparator(SeparatorType.PAREN_CLOSE)
                val thenBranch = parseStatement()
                val elseBranch = if (tokenSource.peek().isKeyword(KeywordType.ELSE)) {
                    tokenSource.consume()
                    parseStatement()
                } else {
                    null
                }
                IfTree(condition, thenBranch, elseBranch, keyword.span.start)
            }

            KeywordType.WHILE -> {
                val keyword = tokenSource.expectKeyword(KeywordType.WHILE)
                tokenSource.expectSeparator(SeparatorType.PAREN_OPEN)
                val condition = parseExpression()
                tokenSource.expectSeparator(SeparatorType.PAREN_CLOSE)
                val body = parseStatement()

                WhileTree(condition, body, keyword.span.start)
            }

            KeywordType.FOR -> {
                val keyword = tokenSource.consume()
                fun parseSimpopt(): StatementTree? {
                    val separator = tokenSource.peekAs<Separator>()
                    return if (separator != null) {
                        null
                    } else {
                        val nextToken = tokenSource.peek()
                        if (nextToken.isType) {
                            parseDeclaration()
                        } else {
                            parseSimple()
                        }
                    }
                }

                tokenSource.expectSeparator(SeparatorType.PAREN_OPEN)
                val initializer = parseSimpopt()
                tokenSource.expectSeparator(SeparatorType.SEMICOLON)
                val condition = parseExpression()
                tokenSource.expectSeparator(SeparatorType.SEMICOLON)
                val step = parseSimpopt()
                tokenSource.expectSeparator(SeparatorType.PAREN_CLOSE)

                val loopBody = parseStatement()

                ForTree(initializer, condition, step, loopBody, keyword.span.start)
            }

            KeywordType.CONTINUE -> {
                val keyword = tokenSource.expectKeyword(KeywordType.CONTINUE)
                tokenSource.expectSeparator(SeparatorType.SEMICOLON)
                ContinueTree(keyword.span)
            }

            KeywordType.BREAK -> {
                val keyword = tokenSource.expectKeyword(KeywordType.BREAK)
                tokenSource.expectSeparator(SeparatorType.SEMICOLON)
                BreakTree(keyword.span)
            }

            KeywordType.RETURN -> {
                val ret = tokenSource.expectKeyword(KeywordType.RETURN)
                val expression = parseExpression()
                tokenSource.expectSeparator(SeparatorType.SEMICOLON)
                return ReturnTree(expression, ret.span.start)
            }

            else -> throw ParseException("expected control statement but got $nextToken")
        }
    }

    private fun parseExpression(): ExpressionTree {
        val lhs = parsePrecedenceExpression(Operator.OperatorType.MAX_PRECEDENCE)
        val operator = this.tokenSource.peekAs<Operator>()
        if (operator == null || operator.type != Operator.OperatorType.TERNARY_QUESTION) {
            return lhs
        }
        this.tokenSource.consume()

        val trueBranch = parseExpression()
        tokenSource.expectOperator(Operator.OperatorType.TERNARY_COLON)
        val falseBranch = parseExpression()

        return TernaryOperationTree(lhs, trueBranch, falseBranch)
    }

    private fun parsePrecedenceExpression(precedence: Int): ExpressionTree {
        if (precedence == Operator.OperatorType.UNARY_PRECEDENCE_LEVEL) {
            return parseUnaryExpression(precedence)
        } else if (precedence == 0) {
            return parseBasicExpression()
        }

        var lhs = parsePrecedenceExpression(precedence - 1)
        while (true) {
            val nextOp = this.tokenSource.peekAs<Operator>()
            if (nextOp != null && precedence in nextOp.type.precedences) {
                this.tokenSource.consume()
                lhs = BinaryOperationTree(lhs, parsePrecedenceExpression(precedence - 1), nextOp.type)
            } else {
                return lhs
            }
        }
    }

    fun parseUnaryExpression(precedence: Int): ExpressionTree {
        val operator = this.tokenSource.peekAs<Operator>()
        if (operator != null && precedence in operator.type.precedences) {
            this.tokenSource.consume()
            val value = parsePrecedenceExpression(precedence)
            return UnaryOperationTree(operator, value)
        }
        return parsePrecedenceExpression(precedence - 1)
    }

    val Token.isBooleanLiteral
        get() = BOOLEAN_LITERAL_KEYWORDS.any { isKeyword(it) }

    fun parseBasicExpression(): ExpressionTree {
        val nextToken = this.tokenSource.peek()
        return when (nextToken) {
            is Separator if nextToken.type == SeparatorType.PAREN_OPEN -> {
                this.tokenSource.consume()
                val expression = parseExpression()
                this.tokenSource.expectSeparator(SeparatorType.PAREN_CLOSE)
                expression
            }

            is NumberLiteral -> {
                this.tokenSource.consume()
                LiteralTree.LiteralIntTree(nextToken.value, nextToken.base, nextToken.span)
            }

            is Keyword if nextToken.isBooleanLiteral -> {
                this.tokenSource.consume()
                LiteralTree.LiteralBoolTree(nextToken)
            }

            is Keyword if nextToken.type in listOf(KeywordType.PRINT, KeywordType.READ, KeywordType.FLUSH) -> {
                val keyword = this.tokenSource.consume() as Keyword
                val arguments = parseArgumentList()
                return CallTree(NameTree(IdentName(keyword.type.name.lowercase()), keyword.span), arguments)
            }

            is Keyword if nextToken.type in listOf(KeywordType.ALLOC, KeywordType.ALLOC_ARRAY) -> {
                val allocKeyword = tokenSource.expectAnyKeyword(listOf(KeywordType.ALLOC, KeywordType.ALLOC_ARRAY))
                tokenSource.expectSeparator(SeparatorType.PAREN_OPEN)
                val allocationType = parseType()

                val allocation = if (allocKeyword.type == KeywordType.ALLOC) {
                    val closingParenthesis = tokenSource.expectSeparator(SeparatorType.PAREN_CLOSE)
                    HeapAllocationTree(allocationType, null, allocKeyword.span merge closingParenthesis.span)
                } else {
                    tokenSource.expectSeparator(SeparatorType.COMMA)
                    val nextToken = tokenSource.consume();
                    val countToken = nextToken as? NumberLiteral
                        ?: throw ParseException("Expected array allocation count, got $nextToken at ${nextToken.span}")
                    val count = countToken.value.toInt(radix = countToken.base)
                    val closingParenthesis = tokenSource.expectSeparator(SeparatorType.PAREN_CLOSE)
                    HeapAllocationTree(allocationType, count, allocKeyword.span merge closingParenthesis.span)
                }

                allocation
            }

            is Identifier -> {
                this.tokenSource.consume()
                val name = name(nextToken)
                if (this.tokenSource.peek().isSeparator(SeparatorType.PAREN_OPEN)) {
                    val arguments = parseArgumentList()
                    CallTree(name, arguments)
                } else {
                    IdentExpressionTree(name)
                }
            }

            else -> throw ParseException("invalid expression starting at $nextToken")
        }
    }

    companion object {
        private val TYPE_KEYWORDS = listOf(KeywordType.INT, KeywordType.BOOL, KeywordType.STRUCT)
        private val CONTROL_KEYWORDS = listOf(
            KeywordType.IF,
            KeywordType.FOR,
            KeywordType.WHILE,
            KeywordType.BREAK,
            KeywordType.CONTINUE,
            KeywordType.RETURN
        )
        private val BOOLEAN_LITERAL_KEYWORDS = listOf(KeywordType.TRUE, KeywordType.FALSE)
        private fun name(ident: Identifier): NameTree {
            return NameTree(Name.forIdentifier(ident), ident.span)
        }
    }
}
