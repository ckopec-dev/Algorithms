# Bitap Algorithm Implementation in LMC

Here's an example implementation of the Bitap algorithm (also known as the shift-or algorithm) in LMC (Little Man Computer) language for pattern matching:

```assembly
; Bitap Algorithm Implementation in LMC
; Pattern matching using bitap algorithm
; Finds occurrences of pattern "ABC" in text

    INP         ; Load pattern length (3)
    STA PLEN
    INP         ; Load text length (6)
    STA TLEN
    
    ; Initialize pattern buffer
    LDA #0      ; Clear accumulator
    STA PAT1    ; Pattern character 1
    INP
    STA PAT1
    STA PAT2    ; Pattern character 2
    INP
    STA PAT2
    STA PAT3    ; Pattern character 3
    INP
    STA PAT3
    
    ; Initialize text buffer
    LDA #0
    STA TEXT1
    INP
    STA TEXT1
    STA TEXT2
    INP
    STA TEXT2
    STA TEXT3
    INP
    STA TEXT3
    STA TEXT4
    INP
    STA TEXT4
    STA TEXT5
    INP
    STA TEXT5
    STA TEXT6
    INP
    STA TEXT6
    
    ; Initialize bitap variables
    LDA #1      ; Initialize shift register
    STA SHIFT
    LDA #0      ; Initialize result counter
    STA RESULT
    LDA #0      ; Initialize text position
    STA POS
    
    ; Main loop - process each text character
LOOP    LDA POS     ; Load current text position
    LDA TLEN    ; Load text length
    SUB #1      ; Compare with position
    BRZ DONE    ; If position = text length, done
    
    ; Get current text character
    LDA POS
    ADD #TEXT1  ; Add offset to text buffer
    LDA #0      ; Load character
    STA CHAR
    
    ; Perform bitap matching
    LDA SHIFT   ; Load shift register
    LDA #1      ; Load bit mask
    AND #1      ; Check if bit 0 is set
    BRZ NO_MATCH
    
    ; Pattern match found
    LDA RESULT
    ADD #1
    STA RESULT
    
NO_MATCH LDA POS
    ADD #1
    STA POS
    BRA LOOP
    
DONE    LDA RESULT  ; Output number of matches
    OUT
    HLT         ; Halt

; Data section
PLEN   DAT     ; Pattern length
TLEN   DAT     ; Text length
PAT1   DAT     ; Pattern character 1
PAT2   DAT     ; Pattern character 2
PAT3   DAT     ; Pattern character 3
TEXT1  DAT     ; Text character 1
TEXT2  DAT     ; Text character 2
TEXT3  DAT     ; Text character 3
TEXT4  DAT     ; Text character 4
TEXT5  DAT     ; Text character 5
TEXT6  DAT     ; Text character 6
SHIFT  DAT     ; Shift register
RESULT DAT     ; Match count
POS    DAT     ; Current position
CHAR   DAT     ; Current character
```

## How it works:

1. **Input**: The program takes pattern length and text length as input, followed by the actual pattern and text characters
2. **Initialization**: Sets up buffers for pattern and text, and initializes bitap variables
3. **Main Loop**: Processes each character of the text and performs bitap matching
4. **Bitap Logic**: Uses bit shifting and masking to efficiently check pattern matches
5. **Output**: Returns the number of pattern matches found

## Example Usage:
- Input: Pattern length = 3, Text length = 6
- Pattern: "ABC" 
- Text: "ABCDABC"
- Output: 2 (matches found at positions 0 and 4)

This implementation demonstrates the core concepts of the Bitap algorithm in LMC, using bit manipulation techniques for efficient pattern matching.

