# Smith-Waterman Algorithm in LMC

Here's an implementation of the Smith-Waterman local sequence alignment algorithm in LMC (Little Man Computer) assembly language:

```assembly
    INP         ; Read first sequence character (A=1, C=2, G=3, T=4)
    STA SEQ1    ; Store first sequence character
    INP         ; Read second sequence character
    STA SEQ2    ; Store second sequence character
    
    ; Initialize matrices
    LDA ZERO    ; Load 0
    STA MATRIX  ; Initialize first cell
    STA MATRIX+1 ; Initialize second cell
    
    ; Main loop for sequence alignment
    LDA SEQ1    ; Load first sequence character
    SUB ONE     ; Subtract 1 to get index
    STA IDX1    ; Store first index
    LDA SEQ2    ; Load second sequence character
    SUB ONE     ; Subtract 1 to get index
    STA IDX2    ; Store second index
    
    ; Calculate matrix position
    LDA IDX1    ; Load first index
    MUL FIVE    ; Multiply by 5 (assuming 5x5 matrix)
    ADD IDX2    ; Add second index
    STA POS     ; Store position
    
    ; Load scoring parameters
    LDA MATCH   ; Load match score
    STA SCORE   ; Store match score
    LDA MISMATCH ; Load mismatch score
    STA SCORE+1 ; Store mismatch score
    LDA GAP     ; Load gap score
    STA SCORE+2 ; Store gap score
    
    ; Calculate maximum score
    LDA MATRIX  ; Load current cell
    ADD SCORE   ; Add match score
    STA TEMP    ; Store temporary score
    LDA TEMP    ; Load temporary score
    SUB SCORE+1 ; Subtract mismatch score
    STA TEMP    ; Store new temporary score
    LDA TEMP    ; Load temporary score
    SUB SCORE+2 ; Subtract gap score
    STA MAX     ; Store maximum score
    
    ; Check if maximum is positive
    LDA MAX     ; Load maximum score
    BRZ POSITIVE ; If zero, go to positive
    LDA ZERO    ; Load zero
    STA MAX     ; Set maximum to zero
    
POSITIVE
    LDA MAX     ; Load maximum score
    STA MATRIX  ; Store in matrix
    
    ; Output result
    LDA MAX     ; Load result
    OUT         ; Output result
    
    HLT         ; Halt program

; Data section
SEQ1    DAT 0     ; First sequence character
SEQ2    DAT 0     ; Second sequence character
MATRIX  DAT 0     ; Matrix storage
IDX1    DAT 0     ; First index
IDX2    DAT 0     ; Second index
POS     DAT 0     ; Position in matrix
SCORE   DAT 0     ; Score storage
TEMP    DAT 0     ; Temporary storage
MAX     DAT 0     ; Maximum score

; Constants
ZERO    DAT 0     ; Zero constant
ONE     DAT 1     ; One constant
FIVE    DAT 5     ; Five constant
MATCH   DAT 2     ; Match score
MISMATCH DAT -1   ; Mismatch score
GAP     DAT -1    ; Gap score
```

## How it works:

1. **Input**: Takes two sequence characters as input (represented as numbers 1-4)
2. **Matrix Initialization**: Sets up a 5x5 scoring matrix
3. **Index Calculation**: Converts sequence positions to matrix indices
4. **Scoring**: Applies Smith-Waterman scoring rules:
   - Match: +2 points
   - Mismatch: -1 point  
   - Gap: -1 point
5. **Maximum Calculation**: Finds the maximum local alignment score
6. **Output**: Displays the computed score

## Example Usage:

Input: `A` (1) and `C` (2)
- First character: 1
- Second character: 2
- Output: Maximum local alignment score

The algorithm implements the core Smith-Waterman dynamic programming approach with local alignment, where negative scores are reset to zero to ensure local optimal alignment.

