structure Parser =
struct

open Mlex.UserDeclarations

fun readFile (file: string): TextIO.instream =
    TextIO.openIn file

fun inputc (is: TextIO.instream): int -> string =
  fn n => TextIO.inputN (is, n)

fun lex2list (lex: unit->lexresult): lexresult list =
  let
    val t = lex()
  in
    case t of
         EOF => nil
       | _ => t::(lex2list lex)
  end

(*
fun interp (lex: unit->lexresult):unit =
  let
    val token = lex()
  in
    case token of
        ADDR(x) => (print("ADDR "^(Int.toString x)^" "); interp lex)
      | SYMBOL(x) => (print("SYMBOL "^x^" "); interp lex)
      | LABEL(x) => (print("LABEL "^x^" "); interp lex)
      | REG(x) => (case x of
                      A => (print("REG A "); interp lex)
                    | M => (print("REG M "); interp lex)
                    | D => (print("REG D "); interp lex))
      | OP(x) => (case x of
                     MINUS => (print("MINUS "); interp lex)
                   | PLUS => (print("PLUS "); interp lex)
                   | AND => (print("AND "); interp lex)
                   | OR => (print("OR "); interp lex)
                   | NOT => (print("NOT "); interp lex))
      | ASSIGN => (print("ASSIGN "); interp lex)
      | BIN(x) => ((if x then print("1 ") else print("0 ")); interp lex)
      | JMP(x) => (print(x ^ " "); interp lex)
      | NEWLINE => (print("\n"); interp lex)
      | EOF => print("EOF\n")
  end
*)

end
