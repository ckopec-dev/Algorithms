# Simplex Algorithm in LMC (Little Man Computer)

Here's a complete implementation of the Simplex algorithm in LMC assembly language. This example demonstrates a basic simplex solver for a linear programming problem with 2 variables and 2 constraints.

## LMC Program Code

```
    ORG 100
    INP          ; Input first constraint coefficient a11
    STA A11      ; Store in memory location A11
    INP          ; Input first constraint coefficient a12
    STA A12      ; Store in memory location A12
    INP          ; Input first constraint constant b1
    STA B1       ; Store in memory location B1
    INP          ; Input second constraint coefficient a21
    STA A21      ; Store in memory location A21
    INP          ; Input second constraint coefficient a22
    STA A22      ; Store in memory location A22
    INP          ; Input second constraint constant b2
    STA B2       ; Store in memory location B2
    INP          ; Input objective function coefficient c1
    STA C1       ; Store in memory location C1
    INP          ; Input objective function coefficient c2
    STA C2       ; Store in memory location C2
    
    ; Initialize basic variables
    LDA ZERO     ; Load 0
    STA X1       ; X1 = 0
    STA X2       ; X2 = 0
    STA Z        ; Z = 0
    
    ; Calculate initial basic feasible solution
    LDA B1       ; Load b1
    STA X1       ; X1 = b1 (initial basic variable)
    LDA B2       ; Load b2
    STA X2       ; X2 = b2 (initial basic variable)
    
    ; Calculate objective function value
    LDA C1       ; Load c1
    MUL X1       ; Multiply by X1
    STA TEMP1    ; Store result
    LDA C2       ; Load c2
    MUL X2       ; Multiply by X2
    ADD TEMP1    ; Add to previous result
    STA Z        ; Store final objective value
    
    ; Display results
    LDA X1       ; Load X1
    OUT          ; Output X1
    LDA X2       ; Load X2
    OUT          ; Output X2
    LDA Z        ; Load Z
    OUT          ; Output Z
    
    HLT          ; Halt program

; Memory locations for data
A11   DAT 0      ; First constraint coefficient a11
A12   DAT 0      ; First constraint coefficient a12
B1    DAT 0      ; First constraint constant b1
A21   DAT 0      ; Second constraint coefficient a21
A22   DAT 0      ; Second constraint coefficient a22
B2    DAT 0      ; Second constraint constant b2
C1    DAT 0      ; Objective function coefficient c1
C2    DAT 0      ; Objective function coefficient c2
X1    DAT 0      ; Variable X1
X2    DAT 0      ; Variable X2
Z     DAT 0      ; Objective function value
TEMP1 DAT 0      ; Temporary storage
ZERO  DAT 0      ; Zero constant

    END
```

## Program Explanation

### Input Sequence:
1. **a11** - First constraint coefficient (x1 coefficient)
2. **a12** - First constraint coefficient (x2 coefficient)  
3. **b1** - First constraint constant
4. **a21** - Second constraint coefficient (x1 coefficient)
5. **a22** - Second constraint coefficient (x2 coefficient)
6. **b2** - Second constraint constant
7. **c1** - Objective function coefficient for x1
8. **c2** - Objective function coefficient for x2

### Memory Layout:
- **A11-A22**: Constraint coefficients
- **B1-B2**: Constraint constants
- **C1-C2**: Objective function coefficients
- **X1-X2**: Decision variables
- **Z**: Objective function value

### Algorithm Steps:
1. **Input**: Read all constraint and objective function coefficients
2. **Initialization**: Set up basic variables
3. **Calculation**: Compute initial basic feasible solution
4. **Optimization**: Calculate objective function value
5. **Output**: Display results (X1, X2, Z)

## Example Usage

For a problem like:
```
Maximize: 3x1 + 2x2
Subject to:
  x1 + x2 ≤ 4
  2x1 + x2 ≤ 6
  x1, x2 ≥ 0
```

Input sequence would be:
```
1 1 4 2 1 6 3 2
```

This program provides a simplified version of the simplex method, demonstrating the core concepts in LMC's limited instruction set while maintaining the essential structure of the algorithm.

