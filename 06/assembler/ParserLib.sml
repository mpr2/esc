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
val _ = pAddr: unit -> int Parser


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
