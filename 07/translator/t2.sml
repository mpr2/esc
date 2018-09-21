datatype 'a Result =
         Success of 'a
         | Failure of string

datatype 'a Parser = Parser of (string -> ('a*string) Result)

fun pchar charToMatch =
    let
      fun innerFn "" = Failure "No more input."
        | innerFn input =
          let
            val first = String.sub(input, 0)
            val remaining = String.extract(input, 1, NONE)
            val err = "Expecting '" ^ (String.str charToMatch)
                      ^ "'. Got '" ^ (String.str first) ^ "'."
          in
            if first = charToMatch
            then Success(charToMatch, remaining)
            else Failure err
          end
    in
      Parser innerFn
    end


fun run parser input =
    let val (Parser innerFn) = parser in
      innerFn input
    end


fun bindP f parser =
    let
      fun innerFn input =
          let val result = run parser input in
            case result of
                Failure err => Failure err
              | Success (value, remaining) =>
                let val parser2 = f value in
                  run parser2 remaining
                end
          end
    in
      Parser innerFn
    end

infix >>=
fun parser >>= f = bindP f parser


fun returnP x =
    let
      fun innerFn input =
          Success (x, input)
    in
      Parser innerFn
    end


infix 3 andThen
fun parser1 andThen parser2 =
    parser1 >>= (fn result1 =>
    parser2 >>= (fn result2 =>
    returnP (result1, result2)))


infix 3 orElse
fun parser1 orElse parser2 =
    let
      fun innerFn input =
          let val result1 = run parser1 input in
            case result1 of
                Success (value, remaining) =>
                Success (value, remaining)
              | Failure err =>
                let val result2 = run parser2 input in
                  case result2 of
                      Success (value2, remaining2) =>
                      Success (value2, remaining2)
                    | Failure err => Failure err
                end
          end
    in
      Parser innerFn
    end


infix 3 mapP
fun parser mapP f =
    parser >>= (returnP o f)


fun choice listOfParsers =
    let
      val h = hd listOfParsers
      val t = tl listOfParsers
    in
      List.foldl op orElse h t
    end


fun anyOf listOfChars =
    (choice o List.map pchar) listOfChars


fun sequence listOfParsers =
    let
      fun concatResults (p2, p1) =
          p1 andThen p2
          mapP (fn(l1, l2) => l1 @ l2)

      val x = List.map (fn parser => parser mapP (fn x => x::nil))
                       listOfParsers
      val h = hd x
      val t = tl x
    in
      List.foldl concatResults h t
    end


fun pstring str =
    let
      val charList = String.explode str
    in
      ((sequence o List.map pchar) charList)
          mapP String.implode
    end


fun parseZeroOrMore parser input =
    let
      val first = run parser input
    in
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


fun many parser =
    let
      fun innerFn input =
          Success (parseZeroOrMore parser input)
    in
      Parser innerFn
    end


fun many1 parser =
    parser      >>= (fn head =>
    many parser >>= (fn tail =>
    returnP (head::tail)))



val input =
    "push constant 7\n"^
    "push constant 8\n"^
    "add"

val init =
    "@256\n"^
    "D=A\n"^
    "@SP\n"^
    "M=D\n"

fun push x =
    "@" ^ (Int.toString x) ^ "\n" ^
    "D=A\n"^
    "@SP\n"^
    "A=M\n"^
    "M=D\n"^
    "@SP\n"^
    "M=M+1\n"

val add =
    "@SP\n"^
    "M=M-1\n"^
    "A=M\n"^
    "D=M\n"^
    "@SP\n"^
    "A=M-1\n"^
    "M=M+D\n"

fun digitListToInt l =
    let
      val s = String.implode l
      val (SOME x) = Int.fromString s
    in
      x
    end

val parsePush = pstring "push"
val parseConstant = pstring "constant"
val parseWhitespace = many (anyOf [#" ", #"\t", #"\n"])
val parseDigit = anyOf
                 [#"0", #"1", #"2", #"3", #"4", #"5", #"6", #"7", #"8", #"9"]
val parseInt = (many1 parseDigit) mapP digitListToInt


val parsePushInst = parsePush
                        andThen parseWhitespace
                        andThen parseConstant
                        andThen parseWhitespace
                        andThen parseInt
                        mapP (fn ((((a,b),c),d),e) => push e)

val parseAdd = (pstring "add") mapP (fn s => add)

val Success(v, r) = run parsePushInst input
val Success(_, r1) = run parseWhitespace r
val Success(v2, r2) = run parsePushInst r1
val Success(_, r3) = run parseWhitespace r2
val Success(v4, r4) = run parseAdd r3;

(print init; print v; print v2; print v4);
