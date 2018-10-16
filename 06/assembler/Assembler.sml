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

val (newSymbolTable, _, newTokenlist) =
    updateSymbolTable symbolTable tokenlist

val (_, _, symbollessTokenlist) =
    resolveSymbols newSymbolTable newTokenlist

val result = run parseSymbolLess symbollessTokenlist;

case result of
    Failure err => print err
  | Success(hack, remaining) => print hack;
