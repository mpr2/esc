(*
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
               
datatype jmp =
         JGT
         | JEQ
         | JGE
         | JLT
         | JNE
         | JLE
         | JMP

datatype Token =
         ADDR of int
         | SYMBOL of string | LABEL of string
         | REG of rg
         | OP of oper
         | ASSIGN
         | BIN of bool
         | JUMP of jmp
         | NEWLINE
         | EOF
*)

open Mlex.UserDeclarations

datatype 'a Result =
         Success of 'a
       | Failure of string

datatype 'a Parser = Parser of (Token list -> ('a * Token list) Result)


fun run parser input =
    let val (Parser parseFn) = parser in
      parseFn input
    end
val _ = run: 'a Parser -> Token list -> ('a * Token list) Result


fun satisfy predicate =
    let
      fun parseFn nil = Failure "No more input."
        | parseFn (h::t) =
          if predicate h
          then Success(h, t)
          else Failure "Unexpected token."
    in
      Parser parseFn
    end
val _ = satisfy: (Token -> bool) -> Token Parser


fun ptoken tok =
    satisfy (fn (tk) => tk = tok)
val _ = ptoken: Token -> Token Parser


fun pAddr () =
    let
      fun parseFn nil = Failure "No more input."
        | parseFn (EOF::t) = Failure "No more input."
        | parseFn (h::t) =
          case h of
              ADDR x => Success(x,t)
            | _ => Failure "Unexpected token."
    in
      Parser parseFn
    end


fun bindP f parser =
    let
      fun parseFn input =
          let val result = run parser input in
            case result of
                Failure err => Failure err
              | Success(value, remaining) =>
                let val parser2 = f value in
                  run parser2 remaining
                end
          end
    in
      Parser parseFn
    end
val _ = bindP: ('a -> 'b Parser) -> 'a Parser -> 'b Parser

infix >>=
fun parser >>= f = bindP f parser


fun returnP x =
    let
      fun parseFn input =
          Success(x, input)
    in
      Parser parseFn
    end
val _ = returnP: 'a -> 'a Parser


fun andThen parser1 parser2 =
    parser1 >>= (fn result1 =>
    parser2 >>= (fn result2 =>
    returnP (result1, result2)))
val _ = andThen: 'a Parser -> 'b Parser -> ('a * 'b) Parser

infix <&>
fun parser1 <&> parser2 = andThen parser1 parser2


fun orElse parser1 parser2 =
    let
      fun parseFn input =
          let val result1 = run parser1 input in
            case result1 of
                Success(value, remaining) =>
                Success(value, remaining)
              | Failure err =>
                let val result2 = run parser2 input in
                  case result2 of
                      Success(value2, remaining2) =>
                      Success(value2, remaining2)
                    | Failure err => Failure err
                end
          end
    in
      Parser parseFn
    end
val _ = orElse: 'a Parser -> 'a Parser -> 'a Parser

infix <|>
fun parser1 <|> parser2 = orElse parser1 parser2


fun mapP f parser =
    parser >>= (returnP o f)
val _ = mapP: ('a -> 'b) -> 'a Parser -> 'b Parser

infix |>
fun f |> parser = mapP f parser


fun choice listOfParsers =
    let val h::t = listOfParsers in
      List.foldl op <|> h t
    end
val _ = choice: 'a Parser list -> 'a Parser


fun anyOf listOfTokens =
    (choice o List.map ptoken) listOfTokens
val _ = anyOf: Token list -> Token Parser


fun sequenceP listOfParsers =
    let
      fun concatResults (p2, p1) =
          (fn (l1, l2) => l1 @ l2)
          |> (p1 <&> p2)

      val h::t = List.map (fn parser => (fn x => x::nil) |> parser)
                          listOfParsers
    in
      List.foldl concatResults h t
    end
val _ = sequenceP: 'a Parser list -> 'a list Parser


fun sequence listOfTokens =
    (sequenceP o List.map ptoken) listOfTokens
val _ = sequence: Token list -> Token list Parser


fun parseZeroOrMore parser input =
    let val first = run parser input in
      case first of
          Failure err => (nil, input)
        | Success(firstVal, remainingAtFirst) =>
          let
            val (subsequent, remaining) =
                parseZeroOrMore parser remainingAtFirst
            val values = firstVal::subsequent
          in
            (values, remaining)
          end
    end
val _ = parseZeroOrMore: 'a Parser -> Token list -> 'a list * Token list


fun many parser =
    let
      fun parseFn input =
          Success (parseZeroOrMore parser input)
    in
      Parser parseFn
    end
val _  = many: 'a Parser -> 'a list Parser


fun many1 parser =
    parser      >>= (fn head =>
    many parser >>= (fn tail =>
    returnP (head::tail)))
val _ = many1: 'a Parser -> 'a list Parser


fun optional parser =
    let
      fun parseFn input =
          let val result = run parser input in
            case result of
                Failure err => Success(NONE, input)
              | Success(value, remaining) =>
                Success(SOME value, remaining)
          end
    in
      Parser parseFn
    end
val _ = optional: 'a Parser -> 'a option Parser


val parseDest =
    let
      fun zeroOrOne NONE = "0"
        | zeroOrOne (SOME x) = "1"

      val pA = ptoken (REG A)
      val pM = ptoken (REG M)
      val pD = ptoken (REG D)
      val pAMD = (sequenceP o List.map optional) [pA, pM, pD]
      val pDest = pAMD <&> (ptoken ASSIGN)

      fun concatResults (lst, x) =
          let val a::m::d::nil = lst in
            (zeroOrOne a) ^ (zeroOrOne d) ^ (zeroOrOne m)
          end
    in
      concatResults |> pDest
    end 
val parseComp =
    let
      val pZero = ptoken (BIN false)
      val pOne = ptoken (BIN true)
      val pMinus = ptoken (OP MINUS)
      val pMinusOne = pMinus <&> pOne
      val pD = ptoken (REG D)
      val pA = ptoken (REG A)
      val pM = ptoken (REG M)
      val pNot = ptoken (OP NOT)
      val pNotD = pNot <&> pD
      val pNotA = pNot <&> pA
      val pNotM = pNot <&> pM
      val pMinusD = pMinus <&> pD
      val pMinusA = pMinus <&> pA
      val pMinusM = pMinus <&> pM
      val pPlus = ptoken (OP PLUS)
      val pDPlusOne = pD <&> pPlus <&> pOne
      val pAPlusOne = pA <&> pPlus <&> pOne
      val pMPlusOne = pM <&> pPlus <&> pOne
      val pDMinusOne = pD <&> pMinusOne
      val pAMinusOne = pA <&> pMinusOne
      val pMMinusOne = pM <&> pMinusOne
      val pDPlusA = pD <&> pPlus <&> pA
      val pDPlusM = pD <&> pPlus <&> pM
      val pDMinusA = pD <&> pMinusA
      val pDMinusM = pD <&> pMinusM
      val pAMinusD = pA <&> pMinusD
      val pMMinusD = pM <&> pMinusD
      val pAnd = ptoken (OP AND)
      val pOr = ptoken (OP OR)
      val pDAndA = pD <&> pAnd <&> pA
      val pDAndM = pD <&> pAnd <&> pM
      val pDOrA = pD <&> pOr <&> pA
      val pDOrM = pD <&> pOr <&> pM
    in
      ((fn x => "0101010") |> pZero)
      <|> ((fn x => "0111111") |> pOne)
      <|> ((fn x => "0111010") |> pMinusOne)
      <|> ((fn x => "0001101") |> pNotD)
      <|> ((fn x => "0110001") |> pNotA)
      <|> ((fn x => "1110001") |> pNotM)
      <|> ((fn x => "0001111") |> pMinusD)
      <|> ((fn x => "0110011") |> pMinusA)
      <|> ((fn x => "1110011") |> pMinusM)
      <|> ((fn x => "0011111") |> pDPlusOne)
      <|> ((fn x => "0110111") |> pAPlusOne)
      <|> ((fn x => "1110111") |> pMPlusOne)
      <|> ((fn x => "0001110") |> pDMinusOne)
      <|> ((fn x => "0110010") |> pAMinusOne)
      <|> ((fn x => "1110010") |> pMMinusOne)
      <|> ((fn x => "0000010") |> pDPlusA)
      <|> ((fn x => "1000010") |> pDPlusM)
      <|> ((fn x => "0010011") |> pDMinusA)
      <|> ((fn x => "1010011") |> pDMinusM)
      <|> ((fn x => "0000111") |> pAMinusD)
      <|> ((fn x => "1000111") |> pMMinusD)
      <|> ((fn x => "0000000") |> pDAndA)
      <|> ((fn x => "1000000") |> pDAndM)
      <|> ((fn x => "0010101") |> pDOrA)
      <|> ((fn x => "1010101") |> pDOrM)
      <|> ((fn x => "0001100") |> pD)
      <|> ((fn x => "0110000") |> pA)
      <|> ((fn x => "1110000") |> pM)
    end

val parseJmp =
    ((fn x => "001") |> ptoken (JUMP JGT))
    <|> ((fn x => "010") |> ptoken (JUMP JEQ))
    <|> ((fn x => "011") |> ptoken (JUMP JGE))
    <|> ((fn x => "100") |> ptoken (JUMP JLT))
    <|> ((fn x => "101") |> ptoken (JUMP JNE))
    <|> ((fn x => "110") |> ptoken (JUMP JLE))
    <|> ((fn x => "111") |> ptoken (JUMP JMP))
        


val parseCInstruction =
    let
      fun zeroOrOne NONE = "000"
        | zeroOrOne (SOME x) = x

      val pDest = zeroOrOne |> (optional parseDest)
      val pJmp = zeroOrOne |> (optional parseJmp)

      fun concatResults lst =
          let val d::c::j::nil = lst in
            "111" ^ c ^ d ^ j ^ "\n"
          end
    in
      concatResults |> (sequenceP [pDest, parseComp, pJmp])
    end


val parseAInstruction =
    let
      fun intToBin n =
          let
            fun aux 0 acc = acc
              | aux num acc =
                aux (num div 2) (Int.toString (num mod 2) ^ acc)
                    
            val s = "0000000000000000" ^ (aux n "")
          in
            (String.extract (s, (size s) - 16, NONE)) ^ "\n"
          end
    in
      intToBin |> pAddr()
    end


val parseInstruction =
    let
      val pNewline = (ptoken NEWLINE) <|> (ptoken EOF)
      fun discard (a, b) = a
    in
      discard |> ((parseAInstruction <|> parseCInstruction) <&> pNewline)
    end


val parseSymbolLess =
    let
      val pNewline = (fn x => "") |> (ptoken NEWLINE)
    in
      concat |> (many1 (pNewline <|> parseInstruction))
    end


val is = Parser.readFile (hd (CommandLine.arguments()))

val lexer = Mlex.makeLexer(Parser.inputc is)

val tokenlist = Parser.lex2list lexer

val result = run parseSymbolLess tokenlist;

case result of
    Failure err => print err
  | Success(v, r) => print v;
