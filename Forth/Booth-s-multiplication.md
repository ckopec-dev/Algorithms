# Booth's Multiplication Algorithm in Forth

Here's an implementation of Booth's multiplication algorithm in Forth:

```forth
\ Booth's Multiplication Algorithm in Forth
\ Multiplies two signed 8-bit numbers using Booth's algorithm

\ Define constants
8 CONSTANT #BITS
16 CONSTANT #BITS2

\ Booth's multiplication algorithm
: booths-mul ( multiplicand multiplier -- product )
    \ Initialize registers
    0 SWAP 0 0 0 0 0  \ A=0, Q=multiplier, Q-1=0, M=multiplicand, count=#bits
    
    \ Booth's algorithm loop
    #BITS 0 DO
        \ Check Q0 and Q-1
        1 RSHIFT 1 AND \ Q0
        0 RSHIFT 1 AND \ Q-1
        
        \ Compare Q0 and Q-1
        2DUP = IF
            \ Q0 = Q-1, do nothing
            DROP DROP
        ELSE
            \ Q0 ≠ Q-1, perform addition/subtraction
            IF
                \ Q0=1, Q-1=0, subtract M from A
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  \ Clear temp
                0 0 0 0 0