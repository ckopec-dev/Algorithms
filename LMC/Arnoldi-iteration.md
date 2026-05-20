# Arnoldi Iteration in LMC

Here's an example implementation of the Arnoldi iteration algorithm in LMC (Little Man Computer) assembly language:

```assembly
; Arnoldi Iteration Algorithm in LMC
; Computes the first few Arnoldi vectors for a matrix

    INP         ; Read matrix dimension n
    STA N       ; Store n
    LDA ZERO    ; Initialize counter
    STA COUNT   ; Set counter to 0

; Main loop for Arnoldi iteration
MAIN_LOOP
    LDA COUNT   ; Load current iteration
    LDA N       ; Load matrix dimension
    SUB ONE     ; Check if we've completed n iterations
    BRZ END_ARNOLDI ; If count = n, end algorithm
    
    ; Initialize v_{j+1} = A * v_j
    LDA COUNT   ; Load current vector index
    STA V_INDEX ; Store index for v_j
    LDA ZERO    ; Initialize result vector
    STA RESULT  ; Clear result
    
    ; Matrix-vector multiplication
    LDA ZERO    ; Initialize row counter
    STA ROW     ; Set row counter to 0
    LDA ZERO    ; Initialize column counter
    STA COL     ; Set column counter to 0
    
MATRIX_MUL
    LDA ROW     ; Load current row
    LDA COL     ; Load current column
    SUB N       ; Check if we've processed all columns
    BRZ NEXT_ROW ; If so, go to next row
    
    ; Load matrix element A[row][col]
    LDA MATRIX  ; Load matrix base address
    LDA ROW     ; Add row offset
    ADD COL     ; Add column offset
    LDA RESULT  ; Load v_j[col]
    MUL RESULT  ; Multiply A[row][col] * v_j[col]
    ADD RESULT  ; Add to result
    STA RESULT  ; Store result
    
    LDA COL     ; Load column counter
    ADD ONE     ; Increment column counter
    STA COL     ; Store incremented column counter
    BRA MATRIX_MUL ; Continue multiplication
    
NEXT_ROW
    LDA ROW     ; Load row counter
    ADD ONE     ; Increment row counter
    STA ROW     ; Store incremented row counter
    LDA ROW     ; Load new row
    LDA N       ; Load matrix dimension
    SUB ONE     ; Check if we've processed all rows
    BRZ COMPUTE_H ; If so, compute Hessenberg matrix
    
    LDA ZERO    ; Reset column counter
    STA COL     ; Set column counter to 0
    BRA MATRIX_MUL ; Continue with next row
    
COMPUTE_H
    ; Compute Hessenberg matrix elements
    LDA COUNT   ; Load current iteration
    ADD ONE     ; Add 1 for H_{j+1,j}
    STA H_INDEX ; Store H matrix index
    
    ; Compute h_{j+1,j} = ||v_{j+1}||_2
    LDA RESULT  ; Load v_{j+1}
    LDA RESULT  ; Square the value
    MUL RESULT  ; Multiply by itself
    ADD H_JJ    ; Add to previous H_{j,j}
    STA H_JJ    ; Store new H_{j,j}
    
    ; Compute H_{j,j+1} = v_{j+1}^T * v_j
    LDA RESULT  ; Load v_{j+1}
    LDA V_J     ; Load v_j
    MUL RESULT  ; Multiply
    STA H_JJ1   ; Store H_{j,j+1}
    
    LDA COUNT   ; Load current count
    ADD ONE     ; Increment count
    STA COUNT   ; Store incremented count
    BRA MAIN_LOOP ; Continue with next iteration

END_ARNOLDI
    LDA ZERO    ; Terminate program
    HLT         ; Halt execution

; Data section
N       DAT 000     ; Matrix dimension
COUNT   DAT 000     ; Iteration counter
V_INDEX DAT 000     ; Vector index
RESULT  DAT 000     ; Result vector
ROW     DAT 000     ; Row counter
COL     DAT 000     ; Column counter
H_INDEX DAT 000     ; Hessenberg matrix index
H_JJ    DAT 000     ; H_{j,j} element
H_JJ1   DAT 000     ; H_{j,j+1} element
V_J     DAT 000     ; Current vector v_j
MATRIX  DAT 000     ; Matrix base address
ZERO    DAT 000     ; Zero constant
ONE     DAT 001     ; One constant

```

## Algorithm Overview

This LMC implementation demonstrates the core concepts of Arnoldi iteration:

1. **Input**: Matrix dimension `n` and matrix `A`
2. **Initialization**: Start with initial vector `v_1`
3. **Iteration**: For each step `j`:
   - Compute `v_{j+1} = A * v_j`
   - Orthogonalize against previous vectors
   - Compute Hessenberg matrix elements
4. **Output**: Arnoldi vectors and Hessenberg matrix

## Key Features

- **Matrix-vector multiplication**: Implements basic linear algebra operations
- **Iteration control**: Uses counters to manage the Arnoldi process
- **Memory management**: Stores intermediate vectors and matrix elements
- **Hessenberg computation**: Builds the Hessenberg matrix during iteration

## Limitations

This is a simplified version that demonstrates the algorithmic structure. A complete implementation would require:
- More sophisticated orthogonalization (Gram-Schmidt)
- Proper memory allocation for vectors
- Advanced matrix storage formats
- Convergence checking mechanisms

The LMC assembly code shows the fundamental steps but would need significant expansion for practical use.

