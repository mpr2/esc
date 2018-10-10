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
