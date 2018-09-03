datatype rg =
    A
  | M
  | D

datatype oper =
    MINUS
  | PLUS
  | AND
  | OR
  | NOT

datatype lexresult =
    ADDR of int
  | SYMBOL of string
  | LABEL of string
  | REG of rg
  | OP of oper
  | ASSIGN
  | BIN of bool
  | JMP of string
  | EOF

val eof = fn () => EOF

%%
%count
%s AT LPARENS RPARENS SEMICOLON;
a = [a-zA-Z\_\.\$];
d = [0-9];
id = {a}({d}|{a})*;
%%
[\ \t\n\r]+       =>  (lex());
\/\/.*            =>  (lex());
@                 =>  (YYBEGIN AT; lex());
<AT>{id}          =>  (YYBEGIN INITIAL; SYMBOL yytext);
<AT>{d}+          =>  (YYBEGIN INITIAL; ADDR (valOf(Int.fromString yytext)));
"("               =>  (YYBEGIN LPARENS; lex());
<LPARENS>{id}     =>  (YYBEGIN RPARENS; LABEL yytext);
<RPARENS>[^")"]   =>  (print "erro label"; lex());
<RPARENS>")"      =>  (YYBEGIN INITIAL; lex());
A                 =>  (REG A);
M                 =>  (REG M);
D                 =>  (REG D);
\-                =>  (OP MINUS);
\+                =>  (OP PLUS);
\&                =>  (OP AND);
\|                =>  (OP OR);
\!                =>  (OP NOT);
\=                =>  (ASSIGN);
0                 =>  (BIN false);
1                 =>  (BIN true);
\;                =>  (YYBEGIN SEMICOLON; lex());
<SEMICOLON>{a}{3} =>  (YYBEGIN INITIAL; JMP yytext);
.                 =>  (print ("erro " ^ (Int.toString (!yylineno)) ^ "\n"); lex());
