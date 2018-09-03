structure Parser =
struct

open Mlex.UserDeclarations

fun readFile (file: string): TextIO.instream =
    TextIO.openIn file

fun inputc (is: TextIO.instream): int -> string =
  fn n => TextIO.inputN (is, n)

fun interp (lex: unit->lexresult):unit =
  let
    val token = lex()
  in
    case token of
        ADDR(x) => (print("ADDR "^(Int.toString x)^"\n"); interp lex)
      | SYMBOL(x) => (print("SYMBOL "^x^"\n"); interp lex)
      | LABEL(x) => (print("LABEL "^x^"\n"); interp lex)
      | REG(x) => (case x of
                      A => (print("REG A\n"); interp lex)
                    | M => (print("REG M\n"); interp lex)
                    | D => (print("REG D\n"); interp lex))
      | OP(x) => (case x of
                     MINUS => (print("MINUS\n"); interp lex)
                   | PLUS => (print("PLUS\n"); interp lex)
                   | AND => (print("AND\n"); interp lex)
                   | OR => (print("OR\n"); interp lex)
                   | NOT => (print("NOT\n"); interp lex))
      | ASSIGN => (print("ASSIGN\n"); interp lex)
      | BIN(x) => ((if x then print("1\n") else print("0\n")); interp lex)
      | JMP(x) => (print(x ^ "\n"); interp lex)
      | EOF => print("EOF\n")
  end

end

