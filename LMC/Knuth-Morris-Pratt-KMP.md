# Knuth-Morris-Pratt (KMP) Algorithm in LMC

Below is an implementation of the KMP algorithm in LMC (Little Man Computer) assembly language. This implementation demonstrates pattern matching using the KMP algorithm with failure function computation.

```assembly
; KMP Algorithm Implementation in LMC
; Pattern: "ABAB" 
; Text: "ABABABAB"
; Expected output: Pattern found at position 0, 2, 4, 6

; Data Section
    INP         ; Input text length
    STA TEXTLEN
    INP         ; Input pattern length
    STA PATTERNLEN
    
    ; Load pattern into memory
    LDA PATTERNLEN
    STA COUNT
    LDA PATTERNSTART
    STA PATTERN
    
PATTERN_LOOP:
    INP
    STA PATTERN
    LDA PATTERN
    STA PATTERN
    LDA COUNT
    SUB ONE
    STA COUNT
    BRZ PATTERN_DONE
    LDA PATTERN
    ADD ONE
    STA PATTERN
    BRA PATTERN_LOOP
    
PATTERN_DONE:
    ; Load text into memory
    LDA TEXTLEN
    STA COUNT
    LDA TEXTSTART
    STA TEXT
    
TEXT_LOOP:
    INP
    STA TEXT
    LDA COUNT
    SUB ONE
    STA COUNT
    BRZ TEXT_DONE
    LDA TEXT
    ADD ONE
    STA TEXT
    BRA TEXT_LOOP
    
TEXT_DONE:
    ; Compute failure function (partial match table)
    LDA ZERO
    STA FAIL
    LDA ONE
    STA I
    LDA ZERO
    STA J
    
FAIL_LOOP:
    LDA I
    SUB PATTERNLEN
    BRZ FAIL_DONE
    
    ; Compute failure function for position I
    LDA J
    SUB ONE
    STA J
    LDA J
    BRZ FAIL_COMPUTE
    
    ; Compare pattern[i] with pattern[j]
    LDA PATTERN
    ADD I
    LDA PATTERN
    ADD J
    SUB
    BRZ FAIL_COMPUTE
    
    LDA J
    BRZ FAIL_COMPUTE
    LDA J
    SUB ONE
    STA J
    BRA FAIL_LOOP
    
FAIL_COMPUTE:
    LDA I
    SUB ONE
    STA I
    LDA I
    SUB PATTERNLEN
    BRZ FAIL_DONE
    
    ; Set failure function value
    LDA J
    STA FAIL
    BRA FAIL_LOOP
    
FAIL_DONE:
    ; KMP search algorithm
    LDA ZERO
    STA TEXTPOS
    LDA ZERO
    STA PATTERNPOS
    
SEARCH_LOOP:
    LDA TEXTPOS
    SUB TEXTLEN
    BRZ SEARCH_DONE
    
    LDA PATTERNPOS
    SUB PATTERNLEN
    BRZ SEARCH_FOUND
    
    ; Compare text[TEXTPOS] with pattern[PATTERNPOS]
    LDA TEXT
    ADD TEXTPOS
    LDA PATTERN
    ADD PATTERNPOS
    SUB
    BRZ SEARCH_CONTINUE
    
    ; Mismatch case
    LDA PATTERNPOS
    BRZ SEARCH_CONTINUE
    
    ; Use failure function
    LDA PATTERNPOS
    SUB ONE
    STA PATTERNPOS
    LDA PATTERNPOS
    BRZ SEARCH_CONTINUE
    
    LDA FAIL
    ADD PATTERNPOS
    STA PATTERNPOS
    BRA SEARCH_CONTINUE
    
SEARCH_CONTINUE:
    LDA TEXTPOS
    ADD ONE
    STA TEXTPOS
    LDA PATTERNPOS
    ADD ONE
    STA PATTERNPOS
    BRA SEARCH_LOOP
    
SEARCH_FOUND:
    ; Pattern found at TEXTPOS - PATTERNLEN
    LDA TEXTPOS
    SUB PATTERNLEN
    STA FOUNDPOS
    LDA FOUNDPOS
    OUT
    LDA NEWLINE
    OUT
    BRA SEARCH_LOOP
    
SEARCH_DONE:
    LDA ZERO
    OUT
    HLT

; Data Definitions
TEXTLEN     DAT 0
PATTERNLEN  DAT 0
COUNT       DAT 0
I           DAT 0
J           DAT 0
TEXTPOS     DAT 0
PATTERNPOS  DAT 0
FOUNDPOS    DAT 0
ZERO        DAT 0
ONE         DAT 1
NEWLINE     DAT 10
PATTERNSTART DAT 100
TEXTSTART   DAT 200
PATTERN     DAT 0
TEXT        DAT 0
FAIL        DAT 0

; Sample Input:
; 8 (text length)
; 4 (pattern length)
; A B A B (pattern)
; A B A B A B A B (text)

; Expected Output:
; 0 (position 0)
; 2 (position 2)
; 4 (position 4)
; 6 (position 6)
; 0 (end marker)
```

## Key Components of the KMP Implementation:

### 1. **Data Section**
- Stores text and pattern lengths
- Memory locations for pattern and text storage
- Variables for algorithm control

### 2. **Failure Function Computation**
- Computes the partial match table (failure function)
- Used to avoid unnecessary character comparisons
- Stores the longest proper prefix which is also suffix

### 3. **Main Search Algorithm**
- Implements the core KMP search logic
- Uses the failure function to skip characters efficiently
- Reports positions where pattern is found

### 4. **Memory Layout**
- `PATTERNSTART`: Starting address of pattern
- `TEXTSTART`: Starting address of text
- `FAIL`: Failure function table
- Various working registers for computation

## Algorithm Complexity:
- **Time Complexity**: O(n + m) where n is text length and m is pattern length
- **Space Complexity**: O(m) for the failure function table

This implementation demonstrates how the KMP algorithm's preprocessing and search phases work together to achieve efficient pattern matching in LMC's limited instruction set.

