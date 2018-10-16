structure Map = RedBlackMapFn(
  struct
  type ord_key = string
  val compare = String.compare
  end
);

fun insertTable (lst: (string*int) list): int Map.map =
    case lst of
        nil => Map.empty
      | h::t => Map.insert(insertTable t, #1 h, #2 h)


val symbolList = [
  ("SP",   0),
  ("LCL",  1),
  ("ARG",  2),
  ("THIS", 3),
  ("THAT", 4),
  ("R0",   0),
  ("R1",   1),
  ("R2",   2),
  ("R3",   3),
  ("R4",   4),
  ("R5",   5),
  ("R6",   6),
  ("R7",   7),
  ("R8",   8),
  ("R9",   9),
  ("R10", 10),
  ("R11", 11),
  ("R12", 12),
  ("R13", 13),
  ("R14", 14),
  ("R15", 15),
  ("SCREEN", 16384),
  ("KBD", 24576)
]

val symbolTable = insertTable symbolList


val parseDestCode =
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

val parseDestInst =
    let
      val pA = ptoken (REG A)
      val pM = ptoken (REG M)
      val pD = ptoken (REG D)
      val pAMD = optionalSequence [pA, pM, pD]
    in
      (fn (x,y) => x@y) |> (pAMD <&> sequence [ASSIGN])
    end


val parseComp =
    let
      val pZero = sequence [BIN false]
      val pOne = sequence [BIN true]
      val pMinusOne = sequence [OP MINUS, BIN true]
      val pD = sequence [REG D]
      val pA = sequence [REG A]
      val pM = sequence [REG M]
      val pNot = sequence [OP NOT]
      val pNotD = sequence [OP NOT, REG D]
      val pNotA = sequence [OP NOT, REG A]
      val pNotM = sequence [OP NOT, REG M]
      val pMinusD = sequence [OP MINUS, REG D]
      val pMinusA = sequence [OP MINUS, REG A]
      val pMinusM = sequence [OP MINUS, REG M]
      val pDPlusOne = sequence [REG D, OP PLUS, BIN true]
      val pAPlusOne = sequence [REG A, OP PLUS, BIN true]
      val pMPlusOne = sequence [REG M, OP PLUS, BIN true]
      val pDMinusOne = sequence [REG D, OP MINUS, BIN true]
      val pAMinusOne = sequence [REG A, OP MINUS, BIN true]
      val pMMinusOne = sequence [REG M, OP MINUS, BIN true]
      val pDPlusA = sequence [REG D, OP PLUS, REG A]
      val pDPlusM = sequence [REG D, OP PLUS, REG M]
      val pDMinusA = sequence [REG D, OP MINUS, REG A]
      val pDMinusM = sequence [REG D, OP MINUS, REG M]
      val pAMinusD = sequence [REG A, OP MINUS, REG D]
      val pMMinusD = sequence [REG M, OP MINUS, REG D]
      val pDAndA = sequence [REG D, OP AND, REG A]
      val pDAndM = sequence [REG D, OP AND, REG M]
      val pDOrA = sequence [REG D, OP OR, REG A]
      val pDOrM = sequence [REG D, OP OR, REG M]
    in
      ((fn x => (x, "0101010")) |> pZero)
      <|> ((fn x => (x, "0111111")) |> pOne)
      <|> ((fn x => (x, "0111010")) |> pMinusOne)
      <|> ((fn x => (x, "0001101")) |> pNotD)
      <|> ((fn x => (x, "0110001")) |> pNotA)
      <|> ((fn x => (x, "1110001")) |> pNotM)
      <|> ((fn x => (x, "0001111")) |> pMinusD)
      <|> ((fn x => (x, "0110011")) |> pMinusA)
      <|> ((fn x => (x, "1110011")) |> pMinusM)
      <|> ((fn x => (x, "0011111")) |> pDPlusOne)
      <|> ((fn x => (x, "0110111")) |> pAPlusOne)
      <|> ((fn x => (x, "1110111")) |> pMPlusOne)
      <|> ((fn x => (x, "0001110")) |> pDMinusOne)
      <|> ((fn x => (x, "0110010")) |> pAMinusOne)
      <|> ((fn x => (x, "1110010")) |> pMMinusOne)
      <|> ((fn x => (x, "0000010")) |> pDPlusA)
      <|> ((fn x => (x, "1000010")) |> pDPlusM)
      <|> ((fn x => (x, "0010011")) |> pDMinusA)
      <|> ((fn x => (x, "1010011")) |> pDMinusM)
      <|> ((fn x => (x, "0000111")) |> pAMinusD)
      <|> ((fn x => (x, "1000111")) |> pMMinusD)
      <|> ((fn x => (x, "0000000")) |> pDAndA)
      <|> ((fn x => (x, "1000000")) |> pDAndM)
      <|> ((fn x => (x, "0010101")) |> pDOrA)
      <|> ((fn x => (x, "1010101")) |> pDOrM)
      <|> ((fn x => (x, "0001100")) |> pD)
      <|> ((fn x => (x, "0110000")) |> pA)
      <|> ((fn x => (x, "1110000")) |> pM)
    end

val parseCompInst =
    (fn (x, y) => x) |> parseComp

val parseCompCode =
    (fn (x, y) => y) |> parseComp


val parseJmp =
    ((fn x => (x,  "001")) |> sequence [JUMP JGT])
    <|> ((fn x => (x,  "010")) |> sequence [JUMP JEQ])
    <|> ((fn x => (x,  "011")) |> sequence [JUMP JGE])
    <|> ((fn x => (x,  "100")) |> sequence [JUMP JLT])
    <|> ((fn x => (x,  "101")) |> sequence [JUMP JNE])
    <|> ((fn x => (x,  "110")) |> sequence [JUMP JLE])
    <|> ((fn x => (x,  "111")) |> sequence [JUMP JMP])

val parseJmpInst =
    (fn (x, y) => x) |> parseJmp

val parseJmpCode =
    (fn (x, y) => y) |> parseJmp


val parseCSymbInst =
    let
      fun flat xs = List.foldr op@ [] xs
      val pDest = flat |> (optionalSequence [parseDestInst])
      val pJmp  = flat |> (optionalSequence [parseJmpInst])
    in
      (fn ((x, y), z) => x@y@z) |> (pDest <&> parseCompInst <&> pJmp)
    end

val parseASymbInst = sequenceP [pAddr() <|> pSymbol()]

val parseLSymbInst = sequenceP [pLabel()]

fun parseSymbInst (symbolTable, linum, lst) =
    let
      fun insert (x, n, t) =
          let val [LABEL label] = x in
            Map.insert(t, label, n)
          end

      val pA = (fn x => (symbolTable, linum + 1, lst@x)) |> parseASymbInst
      val pC = (fn x => (symbolTable, linum + 1, lst@x)) |> parseCSymbInst
      val pL = (fn x => (insert(x, linum, symbolTable),
                         linum, lst)) |> parseLSymbInst

      val pNewline = sequenceP [ptoken NEWLINE <|> ptoken EOF]
      fun keep (a, b) = (#1 a, #2 a, (#3 a)@b)
      val pNL = (fn x => (symbolTable, linum, lst)) |> pNewline
    in
      (keep |> ((pA <|> pC <|> pL) <&> pNewline) <|> pNL)
    end


fun resolveSymbInst (symbolTable, count, lst) =
    let
      fun resolve [ADDR n] = (symbolTable, count, lst@[ADDR n])
        | resolve [SYMBOL s] =
          let val x = Map.find(symbolTable, s) in
            case x of
                SOME n => resolve [ADDR n]
              | NONE => (Map.insert(symbolTable, s, count),
                         count + 1, lst@[ADDR count])
          end

      val pA = resolve |> parseASymbInst
      val pC = (fn x => (symbolTable, count, lst@x)) |> parseCSymbInst

      val pNewline = sequenceP [ptoken NEWLINE <|> ptoken EOF]
      fun keep (a, b) = (#1 a, #2 a, (#3 a)@b)
      val pNL = (fn x => (symbolTable, count, lst)) |> pNewline
    in
      (keep |> ((pA <|> pC) <&> pNewline) <|> pNL)
    end


fun parseSymbols tuple f lst =
    let val first = run (f tuple) lst in
      case first of
          Failure err => tuple
        | Success(firstVal, remainingAtFirst) =>
           parseSymbols firstVal f remainingAtFirst
    end

fun updateSymbolTable symbolTable input =
    (parseSymbols (symbolTable, 0, []) parseSymbInst input)

fun resolveSymbols symbolTable input =
    (parseSymbols (symbolTable, 16, []) resolveSymbInst input)


val parseCInstruction =
    let
      fun zeroOrOne NONE = "000"
        | zeroOrOne (SOME x) = x

      val pDest = zeroOrOne |> (optional parseDestCode)
      val pJmp = zeroOrOne |> (optional parseJmpCode)

      fun concatResults lst =
          let val d::c::j::nil = lst in
            "111" ^ c ^ d ^ j ^ "\n"
          end
    in
      concatResults |> (sequenceP [pDest, parseCompCode, pJmp])
    end


val parseAInstruction =
    let
      fun intToBin (ADDR n) =
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
