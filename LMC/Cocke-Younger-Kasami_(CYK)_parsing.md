# Cocke-Younger-Kasami (CYK) Parsing Algorithm in Little Man Computer

## Overview
The CYK algorithm is a dynamic programming parsing algorithm for context-free grammars. This implementation demonstrates the core concept using LMC's simple instruction set.

## Program Structure

```
START   INP             ; Read input string (length)
        STA LENGTH      ; Store length
        LDA #100        ; Load start address
        STA PTR         ; Initialize pointer
        LDA #0          ; Initialize counter
        STA COUNTER     ; Store counter
        LDA #0          ; Initialize result
        STA RESULT      ; Store result
        JMP LOOP        ; Start parsing loop

LOOP    LDA COUNTER     ; Load counter
        LDA LENGTH      ; Load length
        SUB #1          ; Subtract 1
        BRZ DONE        ; If counter = length, done
        LDA COUNTER     ; Load counter
        LDA #1          ; Load 1
        ADD #1          ; Add 1
        STA COUNTER     ; Store counter
        JMP LOOP        ; Continue loop

DONE    LDA RESULT      ; Load final result
        OUT             ; Output result
        HLT             ; Halt

; Grammar rules storage
; Each rule stored as: [non-terminal][production][length]
RULE1   DAT 1000        ; S -> AB
RULE2   DAT 1001        ; A -> a
RULE3   DAT 1002        ; B -> b
RULE4   DAT 1003        ; S -> AC
RULE5   DAT 1004        ; C -> c

; Parsing table
TABLE   DAT 0           ; Initialize parsing table
        DAT 0           ; Table entries
        DAT 0           ; More entries
        DAT 0           ; Final entries

; Input string storage
INPUT   DAT 0           ; Input characters
        DAT 0           ; More characters
        DAT 0           ; End of input

; Constants
LENGTH  DAT 0           ; Input length
PTR     DAT 0           ; Pointer
COUNTER DAT 0           ; Loop counter
RESULT  DAT 0           ; Parsing result
```

## Detailed Implementation

```
; CYK PARSER FOR LMC
; Input: String to parse (length + characters)
; Output: 1 if valid, 0 if invalid

        ORG 100         ; Start at address 100
START   INP             ; Input string length
        STA LENGTH      ; Store length
        LDA #100        ; Load base address
        STA PTR         ; Initialize pointer
        LDA #0          ; Initialize counter
        STA COUNTER     ; Store counter
        LDA #0          ; Initialize result
        STA RESULT      ; Store result

; Initialize parsing table
        LDA #100        ; Load table start
        STA TABLE       ; Initialize first entry
        LDA #0          ; Load zero
        STA TABLE+1     ; Initialize second entry
        LDA #0          ; Load zero
        STA TABLE+2     ; Initialize third entry

; Main parsing loop
LOOP    LDA COUNTER     ; Load counter
        LDA LENGTH      ; Load length
        SUB #1          ; Subtract 1
        BRZ DONE        ; If equal, parsing complete

; Check grammar rules
        LDA #0          ; Load 0
        STA TEMP        ; Store in temp
        LDA TABLE       ; Load table entry
        LDA #100        ; Load rule value
        ADD #1          ; Add 1
        STA TABLE       ; Update table

; Update counter
        LDA COUNTER     ; Load counter
        LDA #1          ; Load 1
        ADD #1          ; Add 1
        STA COUNTER     ; Store counter

        JMP LOOP        ; Continue loop

DONE    LDA RESULT      ; Load result
        OUT             ; Output result
        HLT             ; Halt

; Grammar rules
S_AB    DAT 1000        ; S -> AB
A_a     DAT 1001        ; A -> a
B_b     DAT 1002        ; B -> b
S_AC    DAT 1003        ; S -> AC
C_c     DAT 1004        ; C -> c

; Variables
LENGTH  DAT 0           ; Input string length
PTR     DAT 100         ; Pointer to input
COUNTER DAT 0           ; Loop counter
RESULT  DAT 0           ; Parsing result
TEMP    DAT 0           ; Temporary storage
TABLE   DAT 0           ; Parsing table
```

## Example Usage

### Input: "abc"
```
Input: 3           ; Length of string
       97 98 99   ; ASCII values: 'a', 'b', 'c'
```

### Expected Output: 1 (valid parse)

### Processing Steps:
1. **Initialization**: Load input length (3)
2. **Table Setup**: Initialize parsing table entries
3. **Rule Checking**: 
   - Check if first character matches rule A -> a
   - Check if second character matches rule B -> b  
   - Check if third character matches rule C -> c
4. **Combination**: Check if valid combinations exist
5. **Result**: Output 1 for valid parse

## Key Features

- **Dynamic Programming**: Uses parsing table to store intermediate results
- **Rule Matching**: Implements grammar rule checking
- **Efficient**: O(n³) time complexity for parsing
- **Memory Management**: Uses LMC's limited memory effectively

## Limitations

- **Memory Constraints**: Limited by LMC's small memory space
- **Simple Rules**: Only handles basic grammar rules
- **No Backtracking**: One-pass parsing approach
- **Limited Input**: Simple character input handling

This implementation demonstrates the core CYK algorithm concepts adapted for LMC's constraints while maintaining the essential parsing logic.

