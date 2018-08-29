// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

@R2
M=0     //R2 = 0


@R0
D=M
@END
D; JEQ  //if R0 == 0 then goto END

@R1
D=M
@END
D; JEQ  //if R1 == 0 then goto END


@R1
D=M
@count
M=D     //count = R1


(LOOP)
@R0
D=M
@R2
M=D+M   //R2 = R2 + R0

@count
M=M-1   //DEC count

D=M
@LOOP
D; JNE  //if count != 0 then goto LOOP


(END)
@END
D; JMP  //End
