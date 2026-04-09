# Strassen's Matrix Multiplication in LMC (Little Man Computer)

Here's an implementation of Strassen's matrix multiplication algorithm in LMC. This is a simplified version that demonstrates the concept with 2x2 matrices.

```assembly
; Strassen's Matrix Multiplication - LMC Implementation
; Multiplies two 2x2 matrices using Strassen's algorithm

; Matrix A (2x2)
; A[0][0] = 1, A[0][1] = 2
; A[1][0] = 3, A[1][1] = 4

; Matrix B (2x2)
; B[0][0] = 5, B[0][1] = 6
; B[1][0] = 7, B[1][1] = 8

; Storage locations
A00   DAT 1      ; Matrix A element [0][0]
A01   DAT 2      ; Matrix A element [0][1]
A10   DAT 3      ; Matrix A element [1][0]
A11   DAT 4      ; Matrix A element [1][1]

B00   DAT 5      ; Matrix B element [0][0]
B01   DAT 6      ; Matrix B element [0][1]
B10   DAT 7      ; Matrix B element [1][0]
B11   DAT 8      ; Matrix B element [1][1]

; Intermediate results storage
M1    DAT 0      ; M1 = (A00 + A11) * (B00 + B11)
M2    DAT 0      ; M2 = (A10 + A11) * B00
M3    DAT 0      ; M3 = A00 * (B01 - B11)
M4    DAT 0      ; M4 = A11 * (B10 - B00)
M5    DAT 0      ; M5 = (A00 + A01) * B11
M6    DAT 0      ; M6 = (A10 - A00) * (B00 + B01)
M7    DAT 0      ; M7 = (A01 - A11) * (B10 + B11)

; Result matrix storage
C00   DAT 0      ; Result matrix element [0][0]
C01   DAT 0      ; Result matrix element [0][1]
C10   DAT 0      ; Result matrix element [1][0]
C11   DAT 0      ; Result matrix element [1][1]

; Main program
    INP      ; Load first number (A00)
    STA A00  ; Store in A00
    INP      ; Load second number (A01)
    STA A01  ; Store in A01
    INP      ; Load third number (A10)
    STA A10  ; Store in A10
    INP      ; Load fourth number (A11)
    STA A11  ; Store in A11
    
    INP      ; Load fifth number (B00)
    STA B00  ; Store in B00
    INP      ; Load sixth number (B01)
    STA B01  ; Store in B01
    INP      ; Load seventh number (B10)
    STA B10  ; Store in B10
    INP      ; Load eighth number (B11)
    STA B11  ; Store in B11

; Calculate M1 = (A00 + A11) * (B00 + B11)
    LDA A00
    ADD A11
    STA TEMP1
    LDA B00
    ADD B11
    STA TEMP2
    LDA TEMP1
    MUL TEMP2
    STA M1

; Calculate M2 = (A10 + A11) * B00
    LDA A10
    ADD A11
    STA TEMP1
    LDA B00
    MUL TEMP1
    STA M2

; Calculate M3 = A00 * (B01 - B11)
    LDA B01
    SUB B11
    STA TEMP2
    LDA A00
    MUL TEMP2
    STA M3

; Calculate M4 = A11 * (B10 - B00)
    LDA B10
    SUB B00
    STA TEMP2
    LDA A11
    MUL TEMP2
    STA M4

; Calculate M5 = (A00 + A01) * B11
    LDA A00
    ADD A01
    STA TEMP1
    LDA B11
    MUL TEMP1
    STA M5

; Calculate M6 = (A10 - A00) * (B00 + B01)
    LDA A10
    SUB A00
    STA TEMP1
    LDA B00
    ADD B01
    MUL TEMP1
    STA M6

; Calculate M7 = (A01 - A11) * (B10 + B11)
    LDA A01
    SUB A11
    STA TEMP1
    LDA B10
    ADD B11
    MUL TEMP1
    STA M7

; Calculate final results using Strassen's formulas
; C00 = M1 + M4 - M5 + M7
    LDA M1
    ADD M4
    SUB M5
    ADD M7
    STA C00

; C01 = M3 + M5
    LDA M3
    ADD M5
    STA C01

; C10 = M2 + M4
    LDA M2
    ADD M4
    STA C10

; C11 = M1 - M2 + M3 + M6
    LDA M1
    SUB M2
    ADD M3
    ADD M6
    STA C11

; Output results
    LDA C00
    OUT
    LDA C01
    OUT
    LDA C10
    OUT
    LDA C11
    OUT

    HLT      ; Halt program

; Temporary storage
TEMP1 DAT 0
TEMP2 DAT 0

    END
```

## Explanation

This LMC implementation demonstrates Strassen's matrix multiplication algorithm for 2×2 matrices:

1. **Input Phase**: Takes 8 inputs representing elements of two 2×2 matrices A and B
2. **Strassen's Algorithm**: Computes 7 products (M1 through M7) using the Strassen formulas
3. **Result Calculation**: Uses the intermediate products to compute the final matrix elements
4. **Output Phase**: Displays the resulting 2×2 matrix

## Key Features

- **Memory Management**: Uses data storage locations for matrix elements and intermediate calculations
- **Arithmetic Operations**: Implements addition, subtraction, and multiplication operations
- **Algorithm Structure**: Follows Strassen's 7-multiplication approach for 2×2 matrices
- **Modular Design**: Separates input, computation, and output phases

## Note

This is a simplified implementation for demonstration purposes. A full implementation would require:
- More sophisticated memory management
- Support for larger matrices
- Proper error handling
- More efficient memory usage

The LMC's limited instruction set makes this a challenging but educational exercise in algorithm implementation.

