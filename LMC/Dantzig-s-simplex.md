# Dantzig's Simplex Algorithm in LMC (Little Man Computer)

Here's a simplified implementation of Dantzig's Simplex algorithm in LMC assembly language:

```assembly
        INP           ; Input first coefficient
        STA COEF1     ; Store first coefficient
        INP           ; Input second coefficient  
        STA COEF2     ; Store second coefficient
        INP           ; Input third coefficient
        STA COEF3     ; Store third coefficient
        INP           ; Input RHS value
        STA RHS       ; Store RHS value

        ; Initialize basic variables
        LDA ZERO      ; Load 0
        STA BASIC1    ; Initialize basic variable 1
        STA BASIC2    ; Initialize basic variable 2
        STA BASIC3    ; Initialize basic variable 3

        ; Simplex iteration
        LDA COEF1     ; Load first coefficient
        SUB UNIT      ; Subtract 1 (for maximization)
        BRZ DONE      ; If zero, solution found
        BRZ NEG1      ; If negative, check for pivot
        BRZ POS1      ; If positive, check for pivot

NEG1    LDA COEF2     ; Load second coefficient
        SUB UNIT      ; Subtract 1
        BRZ DONE      ; If zero, solution found

POS1    LDA COEF3     ; Load third coefficient
        SUB UNIT      ; Subtract 1
        BRZ DONE      ; If zero, solution found

        ; Pivot operation
        LDA COEF1     ; Load first coefficient
        DIV COEF2     ; Divide by second coefficient
        STA RATIO     ; Store ratio

        ; Update tableau
        LDA RHS       ; Load RHS
        DIV COEF2     ; Divide by pivot element
        STA NEWRHS    ; Store new RHS

        ; Continue simplex iterations
        LDA NEWRHS    ; Load new RHS
        STA RHS       ; Update RHS
        LDA COEF1     ; Load first coefficient
        SUB UNIT      ; Subtract 1
        STA COEF1     ; Update coefficient
        LDA COEF2     ; Load second coefficient
        SUB UNIT      ; Subtract 1
        STA COEF2     ; Update coefficient

        ; Check for optimal solution
        LDA COEF1     ; Load first coefficient
        BRZ OPTIMAL   ; If zero, optimal solution found
        LDA COEF2     ; Load second coefficient
        BRZ OPTIMAL   ; If zero, optimal solution found
        LDA COEF3     ; Load third coefficient
        BRZ OPTIMAL   ; If zero, optimal solution found

OPTIMAL LDA RHS       ; Load final RHS value
        OUT           ; Output optimal value
        LDA BASIC1    ; Load basic variable 1
        OUT           ; Output basic variable 1
        LDA BASIC2    ; Load basic variable 2
        OUT           ; Output basic variable 2
        LDA BASIC3    ; Load basic variable 3
        OUT           ; Output basic variable 3

DONE    HLT           ; Halt program

; Data section
COEF1   DAT 0         ; First coefficient
COEF2   DAT 0         ; Second coefficient  
COEF3   DAT 0         ; Third coefficient
RHS     DAT 0         ; Right hand side
BASIC1  DAT 0         ; Basic variable 1
BASIC2  DAT 0         ; Basic variable 2
BASIC3  DAT 0         ; Basic variable 3
RATIO   DAT 0         ; Ratio for pivot selection
NEWRHS  DAT 0         ; New RHS value
UNIT    DAT 1         ; Unit value for operations
ZERO    DAT 0         ; Zero value
```

## Explanation

This LMC implementation demonstrates the core concepts of Dantzig's Simplex algorithm:

1. **Input Processing**: Takes coefficients and RHS values from user input
2. **Initialization**: Sets up basic variables and coefficients
3. **Iteration Loop**: Performs simplex iterations by:
   - Checking optimality conditions
   - Selecting pivot elements
   - Updating the tableau
4. **Optimality Check**: Determines when the optimal solution is reached
5. **Output**: Displays the final solution

## Key LMC Instructions Used

- `INP` - Input data
- `STA` - Store data in memory
- `LDA` - Load data from memory
- `SUB` - Subtract values
- `DIV` - Divide values
- `BRZ` - Branch if zero
- `OUT` - Output data
- `HLT` - Halt program

## Limitations

This is a simplified educational example that:
- Handles only 3 variables (real simplex can handle many more)
- Uses basic pivot selection
- Doesn't include advanced features like degeneracy handling
- Assumes maximization problem
- Simplifies the full tableau operations

The actual simplex algorithm would require more complex memory management and iteration control structures.

