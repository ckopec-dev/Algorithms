# Needleman-Wunsch Algorithm in LMC

The Needleman-Wunsch algorithm is a dynamic programming algorithm for global sequence alignment. Here's an implementation in LMC (Little Man Computer) assembly language:

```assembly
    INP         ; Read first sequence character (ASCII)
    STA SEQ1    ; Store in memory location SEQ1
    INP         ; Read second sequence character (ASCII)
    STA SEQ2    ; Store in memory location SEQ2
    INP         ; Read third sequence character (ASCII)
    STA SEQ3    ; Store in memory location SEQ3
    INP         ; Read fourth sequence character (ASCII)
    STA SEQ4    ; Store in memory location SEQ4
    
    ; Initialize scoring matrix
    LDA ZERO    ; Load 0
    STA MATRIX1 ; Initialize first row
    STA MATRIX2
    STA MATRIX3
    STA MATRIX4
    STA MATRIX5
    
    ; Set up scoring parameters
    LDA MATCH   ; Load match score
    STA SCORE1
    LDA MISMATCH ; Load mismatch score
    STA SCORE2
    LDA GAP     ; Load gap penalty
    STA SCORE3
    
    ; Main alignment loop
    LDA SEQ1    ; Load first sequence character
    STA CHAR1   ; Store in CHAR1
    LDA SEQ2    ; Load second sequence character
    STA CHAR2   ; Store in CHAR2
    
    ; Compare characters
    LDA CHAR1   ; Load first character
    SUB CHAR2   ; Subtract second character
    BRZ MATCH_CASE  ; If zero, characters match
    BRA MISMATCH_CASE ; Otherwise, mismatch
    
MATCH_CASE:
    LDA SCORE1  ; Load match score
    STA SCORE   ; Store in SCORE
    BRA CONTINUE
    
MISMATCH_CASE:
    LDA SCORE2  ; Load mismatch score
    STA SCORE   ; Store in SCORE
    
CONTINUE:
    ; Calculate alignment score
    LDA SCORE   ; Load score
    ADD MATRIX1 ; Add to previous score
    STA RESULT  ; Store result
    
    ; Output result
    LDA RESULT  ; Load result
    OUT         ; Output result
    
    HLT         ; Halt program

    ; Data section
SEQ1    DAT     ; First sequence character
SEQ2    DAT     ; Second sequence character
SEQ3    DAT     ; Third sequence character
SEQ4    DAT     ; Fourth sequence character
CHAR1   DAT     ; First character for comparison
CHAR2   DAT     ; Second character for comparison
SCORE   DAT     ; Current score
RESULT  DAT     ; Final result
MATRIX1 DAT     ; Scoring matrix element 1
MATRIX2 DAT     ; Scoring matrix element 2
MATRIX3 DAT     ; Scoring matrix element 3
MATRIX4 DAT     ; Scoring matrix element 4
MATRIX5 DAT     ; Scoring matrix element 5
SCORE1  DAT     ; Match score (typically +1)
SCORE2  DAT     ; Mismatch score (typically -1)
SCORE3  DAT     ; Gap penalty (typically -1)
ZERO    DAT     ; Zero value (0)
MATCH   DAT     ; Match score value (1)
MISMATCH DAT    ; Mismatch score value (-1)
GAP     DAT     ; Gap penalty value (-1)
```

## How it works:

1. **Input**: Takes four sequence characters as input
2. **Initialization**: Sets up scoring parameters and matrix elements
3. **Comparison**: Compares characters from both sequences
4. **Scoring**: Applies match/mismatch scores based on character comparison
5. **Matrix Update**: Updates the scoring matrix with calculated values
6. **Output**: Produces the alignment score

## Example Usage:

If we input sequences "ACGT" and "ACGT":
- Characters match at each position
- Each match contributes +1 to the score
- Final score would be 4

This implementation demonstrates the core concepts of the Needleman-Wunsch algorithm in LMC assembly language, showing how dynamic programming is used to solve sequence alignment problems.

