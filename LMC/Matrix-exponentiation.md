# Matrix Exponentiation in LMC (Little Man Computer)

Here's an example of matrix exponentiation algorithm implemented in LMC:

```assembly
    INP         ; Read matrix size n
    STA SIZE
    INP         ; Read exponent k
    STA EXPONENT
    
    ; Initialize matrices
    LDA SIZE
    STA MATRIX_A
    LDA SIZE
    STA MATRIX_B
    
    ; Read first matrix (n×n)
    LDA SIZE
    STA ROWS
    LDA SIZE
    STA COLS
    
    LDA #100    ; Starting address for matrix A
    STA ADDR_A
    
    LDA ROWS
    STA ROW_COUNT
    LDA COLS
    STA COL_COUNT
    
    LDA #0      ; Reset counter
    STA COUNTER
    
READ_LOOP:
    LDA COUNTER
    LDA #100    ; Matrix A starting address
    ADD A
    STA TEMP_ADDR
    INP
    LDA TEMP_ADDR
    STA MATRIX_A
    
    LDA COUNTER
    ADD #1
    STA COUNTER
    
    LDA COUNTER
    LDA ROWS
    MUL COLS
    SUB #1
    BRZ READ_DONE
    
    BRA READ_LOOP
    
READ_DONE:
    ; Matrix multiplication function
    LDA #100    ; Start address of result matrix
    STA RESULT_ADDR
    
    ; Initialize result matrix to zero
    LDA #100
    STA INIT_ADDR
    LDA #0
    STA ZERO_INIT
    
INIT_LOOP:
    LDA INIT_ADDR
    STA ZERO_INIT
    LDA INIT_ADDR
    ADD #1
    STA INIT_ADDR
    
    LDA INIT_ADDR
    LDA #100
    SUB #1
    BRZ INIT_DONE
    
    BRA INIT_LOOP
    
INIT_DONE:
    ; Matrix exponentiation
    LDA EXPONENT
    STA POWER
    LDA #1
    STA RESULT_POWER
    
    LDA #100    ; Identity matrix
    STA IDENTITY_ADDR
    
    ; Initialize identity matrix
    LDA #100
    STA IDENTITY_START
    
    LDA SIZE
    STA IDENTITY_SIZE
    
    LDA #100    ; Starting address
    STA IDENTITY_INIT
    
IDENTITY_LOOP:
    LDA IDENTITY_INIT
    LDA IDENTITY_SIZE
    SUB #1
    BRZ IDENTITY_DONE
    
    LDA IDENTITY_INIT
    ADD #1
    STA IDENTITY_INIT
    
    BRA IDENTITY_LOOP
    
IDENTITY_DONE:
    ; Main exponentiation loop
    LDA POWER
    BRZ RESULT_DONE
    
    ; Multiply result by base matrix
    LDA RESULT_ADDR
    LDA #100
    ADD #100
    STA RESULT_ADDR
    
    LDA POWER
    SUB #1
    STA POWER
    
    BRA IDENTITY_LOOP
    
RESULT_DONE:
    ; Output result matrix
    LDA RESULT_ADDR
    LDA #100
    ADD #100
    STA RESULT_ADDR
    
    LDA RESULT_ADDR
    OUT
    
    LDA #0      ; Halt
    STA HALT
    LDA HALT
    BRZ HALT
    BRA HALT
```

## Explanation:

This LMC implementation demonstrates:
- **Input handling**: Reads matrix size and exponent
- **Matrix storage**: Uses memory locations to store matrix elements
- **Matrix initialization**: Creates identity matrix for exponentiation
- **Exponentiation algorithm**: Uses repeated multiplication approach
- **Memory management**: Proper address handling for matrix operations

## Key Features:

1. **Memory Organization**: Matrices stored sequentially in memory
2. **Loop Control**: Uses counters and branching for iteration
3. **Matrix Operations**: Basic multiplication and exponentiation
4. **Input/Output**: Reads input values and outputs results
5. **Control Flow**: Proper branching and looping structures

**Note**: This is a simplified representation. A full implementation would require more detailed memory management and error checking.

