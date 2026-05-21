# Thomas Algorithm in LMC (Little Man Computer)

The Thomas algorithm is a specialized form of Gaussian elimination for solving tridiagonal systems of equations. Here's an implementation in LMC:

## LMC Code Implementation

```
; Thomas Algorithm for Tridiagonal System
; Solves Ax = b where A is tridiagonal
; Input: a, b, c, d arrays (size n)
; Output: solution vector x

START   INP         ; Read n (system size)
        STA SIZE    ; Store size
        LDA ZERO    ; Initialize counter
        STA COUNT   ; Count = 0

; Forward elimination phase
FORWARD LDA COUNT   ; Load counter
        SUB SIZE    ; Compare with size
        BRZ DONE    ; If counter = size, done
        LDA COUNT   ; Load counter
        STA INDEX   ; Store as index
        LDA COUNT   ; Load counter
        ADD ONE     ; counter + 1
        STA INDEX1  ; Store index + 1
        LDA COUNT   ; Load counter
        ADD TWO     ; counter + 2
        STA INDEX2  ; Store index + 2

; Load coefficients
        LDA A       ; Load a[i]
        STA A_VAL   ; Store a[i]
        LDA B       ; Load b[i]
        STA B_VAL   ; Store b[i]
        LDA C       ; Load c[i]
        STA C_VAL   ; Store c[i]
        LDA D       ; Load d[i]
        STA D_VAL   ; Store d[i]

; Forward elimination computation
        LDA B_VAL   ; Load b[i]
        LDA A_VAL   ; Load a[i]
        DIV A_VAL   ; b[i] / a[i]
        STA TEMP1   ; Store quotient
        LDA C_VAL   ; Load c[i]
        LDA TEMP1   ; Load quotient
        MUL TEMP1   ; c[i] * (b[i]/a[i])
        STA TEMP2   ; Store product
        LDA D_VAL   ; Load d[i]
        LDA TEMP1   ; Load quotient
        MUL TEMP1   ; d[i] * (b[i]/a[i])
        STA TEMP3   ; Store product

        LDA B_VAL   ; Load b[i]
        LDA TEMP2   ; Load c[i] * (b[i]/a[i])
        SUB TEMP2   ; b[i] - c[i] * (b[i]/a[i])
        STA B_VAL   ; Store new b[i]
        LDA D_VAL   ; Load d[i]
        LDA TEMP3   ; Load d[i] * (b[i]/a[i])
        SUB TEMP3   ; d[i] - d[i] * (b[i]/a[i])
        STA D_VAL   ; Store new d[i]

        LDA COUNT   ; Load counter
        ADD ONE     ; Increment counter
        STA COUNT   ; Store counter
        BRA FORWARD ; Continue forward elimination

DONE    LDA COUNT   ; Load final counter
        SUB ONE     ; Subtract 1
        STA INDEX   ; Store last index

; Back substitution phase
BACKSUB LDA COUNT   ; Load counter
        SUB ZERO    ; Compare with zero
        BRZ END     ; If counter = 0, done
        LDA COUNT   ; Load counter
        SUB ONE     ; counter - 1
        STA INDEX   ; Store index
        LDA COUNT   ; Load counter
        SUB TWO     ; counter - 2
        STA INDEX1  ; Store index - 1

; Load values for back substitution
        LDA B_VAL   ; Load b[i]
        LDA D_VAL   ; Load d[i]
        LDA C_VAL   ; Load c[i]
        LDA X       ; Load x[i+1]

; Compute x[i] = (d[i] - c[i] * x[i+1]) / b[i]
        LDA C_VAL   ; Load c[i]
        LDA X       ; Load x[i+1]
        MUL X       ; c[i] * x[i+1]
        STA TEMP1   ; Store product
        LDA D_VAL   ; Load d[i]
        LDA TEMP1   ; Load product
        SUB TEMP1   ; d[i] - c[i] * x[i+1]
        STA TEMP2   ; Store difference
        LDA B_VAL   ; Load b[i]
        DIV B_VAL   ; (d[i] - c[i] * x[i+1]) / b[i]
        STA X       ; Store x[i]

        LDA COUNT   ; Load counter
        SUB ONE     ; Decrement counter
        STA COUNT   ; Store counter
        BRA BACKSUB ; Continue back substitution

END     OUT         ; Output result
        HLT         ; Halt

; Data section
SIZE    DAT 000     ; System size
COUNT   DAT 000     ; Loop counter
INDEX   DAT 000     ; Index variable
INDEX1  DAT 000     ; Index + 1
INDEX2  DAT 000     ; Index + 2
A_VAL   DAT 000     ; Temporary a[i]
B_VAL   DAT 000     ; Temporary b[i]
C_VAL   DAT 000     ; Temporary c[i]
D_VAL   DAT 000     ; Temporary d[i]
TEMP1   DAT 000     ; Temporary storage
TEMP2   DAT 000     ; Temporary storage
TEMP3   DAT 000     ; Temporary storage
X       DAT 000     ; Solution variable

; Constants
ZERO    DAT 000
ONE     DAT 001
TWO     DAT 002

; Input arrays (example values)
A       DAT 002     ; a[0] = 2
        DAT 003     ; a[1] = 3
        DAT 004     ; a[2] = 4
        DAT 005     ; a[3] = 5
B       DAT 001     ; b[0] = 1
        DAT 002     ; b[1] = 2
        DAT 003     ; b[2] = 3
        DAT 004     ; b[3] = 4
C       DAT 000     ; c[0] = 0
        DAT 001     ; c[1] = 1
        DAT 002     ; c[2] = 2
        DAT 003     ; c[3] = 3
D       DAT 005     ; d[0] = 5
        DAT 006     ; d[1] = 6
        DAT 007     ; d[2] = 7
        DAT 008     ; d[3] = 8
```

## How it works:

1. **Forward Elimination**: Reduces the tridiagonal system to upper triangular form
2. **Back Substitution**: Solves for the unknowns starting from the last equation
3. **Memory Management**: Uses temporary storage for intermediate calculations
4. **Loop Control**: Implements counter-based loops for iterative processing

## Input Format:
- First input: n (size of system)
- Arrays A, B, C, D (coefficients and constants)
- Each array element is stored sequentially in memory

## Output:
- The solution vector x values

This implementation demonstrates the core concepts of the Thomas algorithm adapted for LMC's limited instruction set and memory structure.

