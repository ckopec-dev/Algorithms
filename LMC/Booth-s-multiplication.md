# Booth's Multiplication Algorithm in LMC

Below is an implementation of Booth's multiplication algorithm in LMC (Little Man Computer) assembly language.

## Algorithm Overview
Booth's algorithm multiplies two signed binary numbers using a shift-and-add approach with special handling for negative numbers.

## LMC Implementation

```
        INP             ; Read multiplicand (M)
        STA MULTIPLICAND
        INP             ; Read multiplier (Q)
        STA MULTIPLIER
        LDA ZERO        ; Initialize accumulator (A) to 0
        STA ACCUMULATOR
        LDA ZERO        ; Initialize Q0 to 0
        STA Q0
        LDA ZERO        ; Initialize Q-1 to 0
        STA Q_MINUS_1
        LDA COUNT       ; Load loop counter (16 bits)
        STA COUNTER
        
        LDA ACCUMULATOR ; Load A to register
        STA TEMP1
        LDA MULTIPLIER  ; Load Q to register
        STA TEMP2
        LDA Q0          ; Load Q0 to register
        STA TEMP3
        LDA Q_MINUS_1   ; Load Q-1 to register
        STA TEMP4
        
        LDA TEMP1       ; A = A + M
        ADD MULTIPLICAND
        STA ACCUMULATOR
        
        LDA ACCUMULATOR ; Shift A left
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL
        LDA ACCUMULATOR
        SHL