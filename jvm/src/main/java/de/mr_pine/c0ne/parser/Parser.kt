package de.mr_pine.c0ne.parser

import de.mr_pine.c0ne.lexer.Identifier
import edu.kit.kastel.vads.compiler.lexer.KeywordType
import de.mr_pine.c0ne.lexer.NumberLiteral
import de.mr_pine.c0ne.lexer.Operator
import de.mr_pine.c0ne.lexer.Separator
import de.mr_pine.c0ne.lexer.Separator.SeparatorType
import de.mr_pine.c0ne.lexer.Token
import edu.kit.kastel.vads.compiler.parser.ParseException
import edu.kit.kastel.vads.compiler.parser.ast.*
import edu.kit.kastel.vads.compiler.parser.symbol.Name
import edu.kit.kastel.vads.compiler.parser.type.BasicType

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
        val returnType = this.tokenSource.expectKeyword(KeywordType.INT)
        val identifier = this.tokenSource.expectIdentifier()
        // Remove with more functions
        if (identifier.asString() != "main") {
            throw ParseException("Only main function allowed")
        }
        this.tokenSource.expectSeparator(SeparatorType.PAREN_OPEN)
        this.tokenSource.expectSeparator(SeparatorType.PAREN_CLOSE)
        val body = parseBlock()
        return FunctionTree(
            TypeTree(BasicType.INT, returnType.span), name(identifier), body
        )
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
        get() = listOf(KeywordType.INT, KeywordType.BOOL).any { isKeyword(it) }

    private fun parseStatement(): StatementTree {
        val statement = if (this.tokenSource.peek().isType) {
            parseDeclaration()
        } else if (this.tokenSource.peek().isKeyword(KeywordType.RETURN)) {
            parseReturn()
        } else {
            parseSimple()
        }
        this.tokenSource.expectSeparator(SeparatorType.SEMICOLON)
        return statement
    }

    private fun parseDeclaration(): StatementTree {
        val type = this.tokenSource.expectKeyword(KeywordType.INT)
        val ident = this.tokenSource.expectIdentifier()
        val init = if (this.tokenSource.peek().isOperator(Operator.OperatorType.ASSIGN)) {
            this.tokenSource.expectOperator(Operator.OperatorType.ASSIGN)
            parseExpression()
        } else {
            null
        }
        return DeclarationTree(TypeTree(BasicType.INT, type.span), name(ident), init)
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
                Operator.OperatorType.ASSIGN, Operator.OperatorType.ASSIGN_DIV, Operator.OperatorType.ASSIGN_MINUS, Operator.OperatorType.ASSIGN_MOD, Operator.OperatorType.ASSIGN_MUL, Operator.OperatorType.ASSIGN_PLUS -> {
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

    private fun parseReturn(): StatementTree {
        val ret = this.tokenSource.expectKeyword(KeywordType.RETURN)
        val expression = parseExpression()
        return ReturnTree(expression, ret.span.start())
    }

    private fun parseExpression(): ExpressionTree {
        var lhs = parseTerm()
        while (true) {
            val nextOp = this.tokenSource.peekAs<Operator>()
            if (nextOp != null && (nextOp.type == Operator.OperatorType.PLUS || nextOp.type == Operator.OperatorType.MINUS)) {
                this.tokenSource.consume()
                lhs = BinaryOperationTree(lhs, parseTerm(), nextOp.type)
            } else {
                return lhs
            }
        }
    }

    private fun parseTerm(): ExpressionTree {
        var lhs = parseFactor()
        while (true) {
            val nextOperator = this.tokenSource.peekAs<Operator>()
            if (nextOperator != null && (nextOperator.type == Operator.OperatorType.MUL || nextOperator.type == Operator.OperatorType.DIV || nextOperator.type == Operator.OperatorType.MOD)) {
                this.tokenSource.consume()
                lhs = BinaryOperationTree(lhs, parseFactor(), nextOperator.type)
            } else {
                return lhs
            }
        }
    }

    private fun parseFactor(): ExpressionTree {
        val next = this.tokenSource.peek()
        return when (next) {
            is Separator if next.type == SeparatorType.PAREN_OPEN -> {
                this.tokenSource.consume()
                val expression = parseExpression()
                this.tokenSource.expectSeparator(SeparatorType.PAREN_CLOSE)
                expression
            }

            is Operator if next.type == Operator.OperatorType.MINUS -> {
                val span = this.tokenSource.consume().span
                NegateTree(parseFactor(), span)
            }

            is Identifier -> {
                this.tokenSource.consume()
                IdentExpressionTree(name(next))
            }

            is NumberLiteral -> {
                this.tokenSource.consume()
                LiteralTree(next.value, next.base, next.span)
            }

            else -> throw ParseException("invalid factor $next")
        }
    }

    companion object {
        private fun name(ident: Identifier): NameTree {
            return NameTree(Name.forIdentifier(ident), ident.span)
        }
    }
}
