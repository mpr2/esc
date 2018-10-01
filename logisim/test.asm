//This is a hack assembly program written to be tested
//by the hack computer implemented in Logisim.
//
//This implementation differs slightly from the original
//one as it only has a 32x16 led display and no keyboard (yet).
//The screen is mapped beggining at the memory address 0x4000,
//and each of the following 32 registers represent a column
//on the screen.
//
//This program is supposed to draw a chess pattern on the screen,
//and invert the pattern each time it is drawn.
//
//TODO: The program is not behaving as expected, probably it is
//some issue with the display driver implementation.


@21845
D=A
@ch
M=D     //ch = 0101010101010101

@21846
D=A
@ss
M=-D     //ss = 1010101010101010


@16416
D=A
@end
M=D     //end = 16416


@even
M=0     //even = 0


(OUTER)
@16384
D=A
@count
M=D     //count = 16384

(LOOP)
@even
D=M
@EVEN
D;JEQ   //if even == 0 then goto EVEN

@even
M=0     //even = 1

@ss
D=M
@count
A=M
M=D     //fill column with "ss"

@CONT
0;JMP   //goto CONT

(EVEN)
@even
M=1     //even = 0

@ch
D=M
@count
A=M
M=D     //fill column with "ch"

(CONT)
@count
MD=M+1
@end
D=M-D   //D = end - count

@OUTER
D;JLT   //if count > end then goto OUTER


@LOOP
0;JMP   //goto LOOP
