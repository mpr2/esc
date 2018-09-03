val is = Parser.readFile (hd (CommandLine.arguments()));

val lexer = Mlex.makeLexer(Parser.inputc is);

Parser.interp lexer;
