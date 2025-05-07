from constraints import *

<start> ::= <whitespace> <program> <whitespace>
<single_whitespace> ::= "\t" | "\n" | " " | <comment>
<comment> ::= <singlelinecomment> | <multilinecomment>
<singlelinecomment> ::= "//" r'[^\n]'* "\n"
<multilinecomment> ::= "/*" <multilinecontent> "*/"
<multilinecontent> ::= <multilinecontent> <multilinecontent> | <multilinecomment> | <simplemultilinecontent>
<simplemultilinecontent> ::= r'.'*
where "/*" not in (str(<simplemultilinecontent>) or "") + "*"
where "*/" not in (str(<simplemultilinecontent>) or "") + "*"
<whitespace> ::= <single_whitespace>+
<program> ::= "int" <whitespace> "main" <whitespace> "(" <whitespace> ")" <whitespace> "{" <whitespace> <stmtswithreturn> <whitespace> "}"
# where enforce_return(<program>)
<stmts> ::= "" | <stmt> <whitespace>? <stmts>
<stmtswithreturn> ::= <stmt> <whitespace>? <stmtswithreturn> | <returnstmt> <stmts>
<stmt> ::= <decl> <whitespace> ";" | <simp> <whitespace> ";" | <returnstmt>
<returnstmt> ::= "return" <whitespace> <exp> ";"
<decl> ::= "int" <whitespace> <ident> | "int" <whitespace> <ident> <whitespace>? "=" <whitespace>? <exp>
<simp> ::= <lvalue> <whitespace>? <asnop> <whitespace>? <exp>
<lvalue> ::= <ident> | "(" <whitespace>? <lvalue> <whitespace>? ")"
<exp> ::= "(" <whitespace>? <exp> <whitespace>? ")" | <intconst> | <ident> | <exp> <whitespace>? <binop> <whitespace>? <exp> | <unop> <whitespace>? <exp>
<intconst> ::= <decnum> | <hexnum>
<unop> ::= "-"
<asnop> ::= "=" | "+=" | "-=" | "*=" | "/=" | "%="
<binop> ::= "+" | "-" | "*" | "/" | "%"
<ident> ::= r'[A-Za-z]' r'[A-Za-z0-9_]'*
<decnum> ::= "0" | r'[1-9]' r'[0-9]'*
where int(str(<decnum>), 10) > 0
where int(str(<decnum>), 10) <= 2147483648
<hexnum> ::= "0" r'[xX]' r'[A-Fa-f0-9]'+
where int(str(<hexnum>)[2:], 16) > 0
where int(str(<hexnum>)[2:], 16) <= 4294967295
