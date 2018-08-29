// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.

(BEGIN)
@24575
D=A
@end
M=D         //end = 24575 (end of screen)


(CLS)
@SCREEN
D=A
@i
M=D         //i = @SCREEN

(LOOPCLS)
@0
D=A
@KBD
D=D-M
@FLS
D; JNE      //if a key is pressed then goto FLS

@end
D=M
@i
D=D-M       //D = end - i

@LOOPCLS
D; JLT      //if i > end then goto LOOPCLS

@0
D=A
@i
A=M
M=D         //RAM[i] = 0 (clear pixels)

@i
M=M+1       //INC i

@LOOPCLS
D; JMP      //goto LOOPCLS



(FLS)
@SCREEN
D=A
@i
M=D         //i = @SCREEN

(LOOPFLS)
@0
D=A
@KBD
D=D-M
@CLS
D; JEQ      //if no key is pressed then goto CLS

@end
D=M
@i
D=D-M       //D = end - i

@LOOPFLS
D; JLT      //if i > end then goto LOOPFLS

@0
D=A
D=!D
@i
A=M
M=D         //RAM[i] = -1 (fill pixels)

@i
M=M+1       //INC i


@LOOPFLS
D; JMP      //goto LOOPFLS
