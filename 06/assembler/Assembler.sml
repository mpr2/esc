fun readFile (file: string): TextIO.instream =
    TextIO.openIn file

fun inputc (is: TextIO.instream): int -> string =
  fn n => TextIO.inputN (is, n)

fun lex2list (lex: unit->lexresult): lexresult list =
  let
    val t = lex()
  in
    case t of
         EOF => EOF::nil
       | _ => t::(lex2list lex)
  end


val is = readFile (hd (CommandLine.arguments()))
val lexer = Mlex.makeLexer(inputc is)
val tokenlist = lex2list lexer



fun printtokenlist tokenlist =
    case tokenlist of
        nil => ()
      | h::t =>
        case h of
            ADDR x => (print "ADDR "; printtokenlist t)
          | SYMBOL x => (print "SYMBOL "; printtokenlist t)
          | LABEL x => (print "LABEL "; printtokenlist t)
          | REG x => (print "REG "; printtokenlist t)
          | OP x => (print "OP "; printtokenlist t)
          | ASSIGN => (print "ASSIGN "; printtokenlist t)
          | BIN x => (print "BIN "; printtokenlist t)
          | JUMP x => (print "JUMP "; printtokenlist t)
          | NEWLINE => (print "\n"; printtokenlist t)
          | EOF => ()





val (newSymbolTable, _, newTokenlist) =
    updateSymbolTable symbolTable tokenlist

val (_, _, symbollessTokenlist) =
    resolveSymbols newSymbolTable newTokenlist

val result = run parseSymbolLess symbollessTokenlist;

case result of
    Failure err => print (err ^ "\n")
  | Success(hack, remaining) => print hack;
