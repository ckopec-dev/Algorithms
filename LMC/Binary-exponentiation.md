# Binary Exponentiation in LMC

Here's an implementation of binary exponentiation algorithm in LMC (Little Man Computer) assembly language:

```assembly
; Binary Exponentiation Algorithm in LMC
; Calculates base^exponent using binary exponentiation method
; Input: Base in memory location 10, Exponent in location 11
; Output: Result in location 12

    INP         ; Input base
    STA 10      ; Store base
    INP         ; Input exponent
    STA 11      ; Store exponent
    LDA 11      ; Load exponent
    BRZ DONE    ; If exponent is 0, result is 1
    LDA ONE     ; Load 1
    STA 12      ; Initialize result = 1
    LDA 10      ; Load base
    STA 13      ; Store base in temp
    LDA 11      ; Load exponent
    STA 14      ; Store exponent in temp
    
LOOP    LDA 14      ; Load current exponent
    BRZ DONE    ; If exponent is 0, we're done
    LDA 14      ; Load exponent
    SUB TWO     ; Subtract 2 (check if even)
    BRZ EVEN    ; If result is 0, exponent is even
    LDA 12      ; Load result
    MUL 13      ; Multiply by base
    STA 12      ; Store result
    LDA 14      ; Load exponent
    SUB ONE     ; Subtract 1
    STA 14      ; Update exponent
    BRZ DONE    ; If exponent is 0, we're done
    BR LOOP     ; Continue loop
    
EVEN    LDA 13      ; Load base
    MUL 13      ; Square the base
    STA 13      ; Store new base
    LDA 14      ; Load exponent
    DIV TWO     ; Divide by 2 (shift right)
    STA 14      ; Update exponent
    BR LOOP     ; Continue loop
    
DONE    LDA 12      ; Load final result
    OUT         ; Output result
    HLT         ; Halt

; Constants
ONE   DAT 1
TWO   DAT 2
```

## How it works:

1. **Input**: Takes base and exponent from user input
2. **Initialization**: Sets result = 1, stores base and exponent
3. **Binary Exponentiation Logic**:
   - If exponent is odd: multiply result by current base, decrement exponent
   - If exponent is even: square the base, halve the exponent
4. **Loop**: Continue until exponent becomes 0
5. **Output**: Display final result

## Example execution:
For base=3, exponent=4:
- 3^4 = 3^2 × 3^2 = 9 × 9 = 81

The algorithm uses the binary representation of the exponent to minimize multiplications, making it much more efficient than naive repeated multiplication.

