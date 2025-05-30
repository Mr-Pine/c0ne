module Compile.Parser
  ( parseAST
  ) where

import           Compile.AST (AST(..), Expr(..), Op(..), Stmt(..), IntLiteral (DecLit, HexLit))
import           Error (L1ExceptT, parserFail)

import           Control.Monad.Combinators.Expr
import           Control.Monad.IO.Class (liftIO)
import           Data.Functor (void)
import           Data.Void (Void)

import Text.Megaparsec
    ( (<|>),
      (<?>),
      getSourcePos,
      oneOf,
      parse,
      errorBundlePretty,
      between,
      many,
      some,
      Parsec,
      MonadParsec(try, eof, lookAhead, notFollowedBy) )
import Text.Megaparsec.Char
    ( char, string, digitChar )
import qualified Text.Megaparsec.Char.Lexer as L

parseAST :: FilePath -> L1ExceptT AST
parseAST path = do
  text <- liftIO $ readFile path
  case parse astParser path text of
    Left err -> parserFail $ errorBundlePretty err
    Right ast -> return ast

type Parser = Parsec Void String

astParser :: Parser AST
astParser = do
  sc
  -- this parses `int main()` literally, like in the L1 grammar
  reserved "int"
  reserved "main"
  parens $ pure ()
  mainBlock <- braces $ do
    pos <- getSourcePos
    stmts <- many stmt
    return $ Block stmts pos
  eof
  return mainBlock

stmt :: Parser Stmt
stmt = do
  s <- try decl <|> try simp <|> ret
  semi
  return s

decl :: Parser Stmt
decl = try declInit <|> declNoInit

declNoInit :: Parser Stmt
declNoInit = do
  pos <- getSourcePos
  reserved "int"
  name <- identifier
  return $ Decl name pos

declInit :: Parser Stmt
declInit = do
  pos <- getSourcePos
  reserved "int"
  name <- identifier
  void $ symbol "="
  e <- expr
  return $ Init name e pos

simp :: Parser Stmt
simp = do
  pos <- getSourcePos
  name <- lvalue
  op <- asnOp
  e <- expr
  return $ Asgn name op e pos

asnOp :: Parser (Maybe Op)
asnOp = do
  op <- operator
  case op of
    "+=" -> pure (Just Add)
    "*=" -> pure (Just Mul)
    "-=" -> pure (Just Sub)
    "/=" -> pure (Just Div)
    "%=" -> pure (Just Mod)
    "=" -> pure Nothing
    x -> fail $ "Nonexistent assignment operator: " ++ x
  <?> "assignment operator"

ret :: Parser Stmt
ret = do
  pos <- getSourcePos
  reserved "return"
  e <- expr
  return $ Ret e pos

expr' :: Parser Expr
expr' = parens expr <|> intExpr <|> identExpr

intExpr :: Parser Expr
intExpr = do
  pos <- getSourcePos
  val <- number
  return $ IntExpr val pos

identExpr :: Parser Expr
identExpr = do
  pos <- getSourcePos
  name <- identifier
  return $ Ident name pos

opTable :: [[Operator Parser Expr]]
opTable =
  [ [Prefix manyUnaryOp]
  , [ InfixL (BinExpr Mul <$ symbol "*")
    , InfixL (BinExpr Div <$ symbol "/")
    , InfixL (BinExpr Mod <$ symbol "%")
    ]
  , [InfixL (BinExpr Add <$ symbol "+"), InfixL (BinExpr Sub <$ symbol "-")]
  ]
  where
    -- this allows us to parse `---x` as `-(-(-x))`
    -- makeExprParser doesn't do this by default
    manyUnaryOp = foldr1 (.) <$> some (UnExpr Neg <$ symbol "-")

expr :: Parser Expr
expr = try (makeExprParser expr' opTable) <?> "expression"

-- Lexer starts here, probably worth moving to its own file at some point
sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    space1 = void $ char ' ' <|> char '\n' <|> char '\r' <|> char '\t'
    lineComment = L.skipLineComment "//"
    blockComment = L.skipBlockCommentNested "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

semi :: Parser ()
semi = void $ symbol ";"

number :: Parser IntLiteral
number = try hexadecimal <|> decimal <|> decimal0 <?> "number"

decimal :: Parser IntLiteral
decimal = DecLit <$> do
    void $ lookAhead $ oneOf "123456789"
    lexeme L.decimal

decimal0 :: Parser IntLiteral
decimal0 = DecLit 0 <$ char '0' <* sc

hexadecimal :: Parser IntLiteral
hexadecimal = HexLit <$> (char '0' >> oneOf ['x','X'] >> lexeme L.hexadecimal)

reserved :: String -> Parser ()
reserved w = void $ lexeme $ (string w <* notFollowedBy identLetter)

reservedWords :: [String]
reservedWords =
  [ "alloc"
  , "alloc_array"
  , "assert"
  , "bool"
  , "break"
  , "char"
  , "continue"
  , "else"
  , "false"
  , "for"
  , "if"
  , "int"
  , "NULL"
  , "print"
  , "read"
  , "return"
  , "string"
  , "struct"
  , "true"
  , "void"
  , "while"
  ]

-- Operations
opStart :: Parser Char
opStart = oneOf "=+-*/%&^|<>!~"

opLetter :: Parser Char
opLetter = oneOf "=&|<>"

operator :: Parser String
operator = lexeme ((:) <$> opStart <*> many opLetter)

-- Identifiers
identStart :: Parser Char
identStart = asciiLetter <|> char '_'

identLetter :: Parser Char
identLetter = asciiAlphaNum <|> char '_'

asciiLetter :: Parser Char
asciiLetter = oneOf (['A'..'Z'] ++ ['a'..'z'])
asciiAlphaNum :: Parser Char
asciiAlphaNum = asciiLetter <|> digitChar

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> identStart <*> many identLetter
    check x =
      if x `elem` reservedWords
        then fail (x ++ " is reserved")
        else return x

lvalue :: Parser String
lvalue = try identifier <|> parens lvalue <?> "lvalue"
