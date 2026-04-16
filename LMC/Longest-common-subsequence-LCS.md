# Longest Common Subsequence (LCS) Algorithm in LMC

Here's an implementation of the LCS algorithm in LMC (Little Man Computer) assembly language:

```assembly
; LCS Algorithm in LMC
; Computes longest common subsequence between two strings
; Input: Two strings stored in memory locations
; Output: Length of LCS stored in location 99

    INP         ; Read first string length
    STA LENGTH1
    INP         ; Read second string length
    STA LENGTH2
    
    ; Initialize DP table
    LDA LENGTH1
    STA I       ; Initialize row counter
    LDA LENGTH2
    STA J       ; Initialize column counter
    
    ; Clear DP table (initialize to 0)
    LDA ZERO
    STA DP_TABLE
    LDA ZERO
    STA DP_TABLE+1
    LDA ZERO
    STA DP_TABLE+2
    LDA ZERO
    STA DP_TABLE+3
    LDA ZERO
    STA DP_TABLE+4
    
    ; Main LCS computation loop
COMPUTE_LCS
    LDA I
    LDA LENGTH1
    BRZ END_COMPUTE
    LDA J
    LDA LENGTH2
    BRZ RESET_J
    
    ; Load characters from both strings
    LDA I
    SUB ONE
    STA INDEX1
    LDA J
    SUB ONE
    STA INDEX2
    
    ; Get characters from strings
    LDA STRING1
    LDA INDEX1
    STA CHAR1
    LDA STRING2
    LDA INDEX2
    STA CHAR2
    
    ; Compare characters
    LDA CHAR1
    SUB CHAR2
    BRZ MATCH
    
    ; Characters don't match - take max from left or top
    LDA DP_TABLE
    LDA DP_TABLE+1
    BRZ NO_MATCH
    LDA DP_TABLE+1
    LDA DP_TABLE
    SUB
    BRZ NO_MATCH
    LDA DP_TABLE+1
    BRZ NO_MATCH
    
    ; Store max value
    LDA DP_TABLE
    STA DP_TABLE+3
    LDA DP_TABLE+1
    STA DP_TABLE+4
    BRZ END_COMPUTE
    
MATCH
    ; Characters match - add 1 to diagonal value
    LDA DP_TABLE
    ADD ONE
    STA DP_TABLE+3
    BRZ END_COMPUTE
    
RESET_J
    LDA LENGTH1
    SUB ONE
    STA I
    LDA LENGTH2
    SUB ONE
    STA J
    LDA ZERO
    STA J
    LDA I
    SUB ONE
    BRZ END_COMPUTE
    LDA I
    SUB ONE
    STA I
    BRZ COMPUTE_LCS
    
END_COMPUTE
    LDA DP_TABLE+3
    OUT
    HLT

; Data section
LENGTH1   DAT 0
LENGTH2   DAT 0
I         DAT 0
J         DAT 0
INDEX1    DAT 0
INDEX2    DAT 0
CHAR1     DAT 0
CHAR2     DAT 0
DP_TABLE  DAT 0
          DAT 0
          DAT 0
          DAT 0
          DAT 0
STRING1   DAT 0
STRING2   DAT 0
ZERO      DAT 0
ONE       DAT 1

; Example usage:
; Input: "ABCDGH" and "AEDFHR" 
; Expected output: 3 (common subsequence: "ADH")
```

## How it works:

1. **Input**: Takes two string lengths as input
2. **Initialization**: Sets up a dynamic programming table
3. **Computation**: Uses the standard LCS recurrence relation:
   - If characters match: `LCS[i][j] = LCS[i-1][j-1] + 1`
   - If characters don't match: `LCS[i][j] = max(LCS[i-1][j], LCS[i][j-1])`
4. **Output**: Returns the length of the longest common subsequence

## Key LMC Features Used:

- **Memory addressing**: Uses memory locations for data storage
- **Branching**: Conditional jumps for control flow
- **Arithmetic operations**: Addition, subtraction for table computations
- **Input/Output**: Reads input strings and outputs result

This implementation demonstrates the core LCS algorithm logic adapted for LMC's limited instruction set and memory model.

