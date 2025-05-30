package de.mr_pine.c0ne.parser

import de.mr_pine.c0ne.lexer.*
import de.mr_pine.c0ne.lexer.Separator.SeparatorType
import de.mr_pine.c0ne.parser.ast.*
import de.mr_pine.c0ne.parser.symbol.Name
import de.mr_pine.c0ne.parser.type.BasicType

class Parser(private val tokenSource: TokenSource) {
    fun parseProgram(): ProgramTree {
        return ProgramTree(parseTopLevelElements())
    }

    private fun parseTopLevelElements(): MutableList<FunctionTree> {
        val topLevelElements = mutableListOf<FunctionTree>()
        while (tokenSource.hasMore()) {
            val function = parseFunction()
            topLevelElements.add(function)
        }
        if (topLevelElements.size != 1) {
            throw ParseException("L2 program should contain exactly one function")
        }
        return topLevelElements
    }

    private fun parseFunction(): FunctionTree {
        val type = parseType()
        val identifier = this.tokenSource.expectIdentifier()
        // Remove with more functions
        if (identifier.asString() != "main") {
            throw ParseException("Only main function allowed")
        } else if (type.type != BasicType.Integer) {
            throw ParseException("Main function must return integer")
        }
        this.tokenSource.expectSeparator(SeparatorType.PAREN_OPEN)
        this.tokenSource.expectSeparator(SeparatorType.PAREN_CLOSE)
        val body = parseBlock()
        return FunctionTree(type, name(identifier), body)
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

    private fun parseDeclaration(): StatementTree {
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
        val type = when (typeKeyword.type) {
            KeywordType.INT -> BasicType.Integer
            KeywordType.BOOL -> BasicType.Boolean
            else -> throw ParseException("expected type but got $typeKeyword")
        }

        return TypeTree(type, typeKeyword.span)
    }

    private fun parseSimple(): StatementTree {
        val lValue = parseLValue()
        val assignmentOperator = parseAssignmentOperator()
        val expression = parseExpression()
        return AssignmentTree(lValue, assignmentOperator, expression)
    }

    private fun parseAssignmentOperator(): Operator {
        val operator = tokenSource.peekAs<Operator>()
        if (operator != null) {
            return when (operator.type) {
                Operator.OperatorType.ASSIGN, Operator.OperatorType.ASSIGN_DIV, Operator.OperatorType.ASSIGN_MINUS, Operator.OperatorType.ASSIGN_MOD, Operator.OperatorType.ASSIGN_MUL, Operator.OperatorType.ASSIGN_PLUS, Operator.OperatorType.ASSIGN_OR, Operator.OperatorType.ASSIGN_AND, Operator.OperatorType.ASSIGN_XOR -> {
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

            is Identifier -> {
                this.tokenSource.consume()
                IdentExpressionTree(name(nextToken))
            }

            else -> throw ParseException("invalid expression starting at $nextToken")
        }
    }

    companion object {
        private val TYPE_KEYWORDS = listOf(KeywordType.INT, KeywordType.BOOL)
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
