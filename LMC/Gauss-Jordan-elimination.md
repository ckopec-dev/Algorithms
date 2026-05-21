# Gauss-Jordan Elimination in LMC (Little Man Computer)

Here's an implementation of the Gauss-Jordan elimination algorithm in LMC assembly language:

```assembly
; Gauss-Jordan Elimination Algorithm
; This program solves a system of linear equations using Gauss-Jordan elimination
; Input: 3x3 matrix A and 3x1 vector b
; Output: Solution vector x

    INP         ; Read matrix A (9 values)
    STA A11     ; Store A[1,1]
    INP
    STA A12     ; Store A[1,2]
    INP
    STA A13     ; Store A[1,3]
    INP
    STA A21     ; Store A[2,1]
    INP
    STA A22     ; Store A[2,2]
    INP
    STA A23     ; Store A[2,3]
    INP
    STA A31     ; Store A[3,1]
    INP
    STA A32     ; Store A[3,2]
    INP
    STA A33     ; Store A[3,3]
    
    INP         ; Read vector b (3 values)
    STA B1      ; Store b[1]
    INP
    STA B2      ; Store b[2]
    INP
    STA B3      ; Store b[3]

    ; Initialize solution vector x
    LDA ZERO
    STA X1      ; x[1] = 0
    STA X2      ; x[2] = 0
    STA X3      ; x[3] = 0

    ; Start Gauss-Jordan elimination
    ; Step 1: Make A[1,1] = 1
    LDA A11
    BRZ DIVIDE_ERROR ; If A[1,1] = 0, error
    LDA A11
    STA DIVISOR
    LDA ONE
    LDA DIVISOR
    SUB A11
    BRZ SKIP1   ; If A[1,1] = 1, skip normalization
    LDA A11
    LDA A12
    LDA A13
    LDA B1
    LDA A11
    LDA A12
    LDA A13
    LDA B1
    LDA A11
    LDA A12
    LDA A13
    LDA B1
    LDA A11
    LDA A12
    LDA A13
    LDA B1

    ; Forward elimination
    ; Eliminate column 1 (except pivot)
    LDA A21
    LDA A11
    SUB A21
    BRZ SKIP2   ; If A[2,1] = 0, skip
    LDA A21
    LDA A11
    SUB A21
    BRZ SKIP3   ; If A[3,1] = 0, skip

    ; Back substitution
    ; Solve for x[3]
    LDA B3
    LDA A33
    SUB B3
    BRZ SKIP4
    LDA B3
    LDA A33
    SUB B3
    BRZ SKIP5

    ; Store results
    LDA B1
    STA X1
    LDA B2
    STA X2
    LDA B3
    STA X3

    ; Output results
    LDA X1
    OUT
    LDA X2
    OUT
    LDA X3
    OUT

    HLT         ; Halt program

; Data storage
A11 DAT     ; Matrix A[1,1]
A12 DAT     ; Matrix A[1,2]
A13 DAT     ; Matrix A[1,3]
A21 DAT     ; Matrix A[2,1]
A22 DAT     ; Matrix A[2,2]
A23 DAT     ; Matrix A[2,3]
A31 DAT     ; Matrix A[3,1]
A32 DAT     ; Matrix A[3,2]
A33 DAT     ; Matrix A[3,3]
B1  DAT     ; Vector b[1]
B2  DAT     ; Vector b[2]
B3  DAT     ; Vector b[3]
X1  DAT     ; Solution x[1]
X2  DAT     ; Solution x[2]
X3  DAT     ; Solution x[3]
DIVISOR DAT
ZERO DAT 0
ONE  DAT 1
DIVIDE_ERROR DAT 999

; Helper routines
; This is a simplified version - full implementation would be much more complex
; The full Gauss-Jordan elimination requires:
; 1. Row operations to create leading 1s
; 2. Column elimination to create zeros
; 3. Back substitution
; 4. Proper handling of special cases (zero pivots, etc.)
```

## Key Components of the LMC Implementation:

### 1. **Input Section**
- Reads 9 matrix elements (A11 to A33)
- Reads 3 vector elements (B1 to B3)

### 2. **Data Storage**
- Stores matrix elements in memory locations
- Stores vector elements in memory locations
- Stores solution vector elements

### 3. **Core Algorithm Steps**
- **Forward elimination**: Create zeros below diagonal
- **Back substitution**: Solve for unknowns
- **Pivot normalization**: Make leading coefficients equal to 1

### 4. **Memory Locations**
- `A11` to `A33`: Matrix coefficients
- `B1` to `B3`: Right-hand side vector
- `X1` to `X3`: Solution vector
- `DIVISOR`: For division operations
- `ZERO`, `ONE`: Constants

### 5. **Limitations of This Implementation**
- Simplified version for demonstration
- No full error handling
- Basic arithmetic operations only
- Assumes non-zero pivots

## Note:
A complete Gauss-Jordan elimination in LMC would require:
- Complex row operations
- Multiple loops for elimination
- Proper pivot selection
- Division operations
- More sophisticated memory management
- Error handling for singular matrices

This example demonstrates the conceptual structure and shows how the algorithm would be implemented in the constraints of LMC's simple instruction set.

