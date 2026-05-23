# Boyer-Moore Algorithm Implementation in LMC

Here's a simplified implementation of the Boyer-Moore string search algorithm in LMC (Little Man Computer) assembly language:

```assembly
        INP             ; Input pattern character 1
        STA PATTERN1    ; Store pattern character 1
        INP             ; Input pattern character 2
        STA PATTERN2    ; Store pattern character 2
        INP             ; Input pattern character 3
        STA PATTERN3    ; Store pattern character 3
        
        LDA COUNT       ; Load count of text characters
        STA COUNT2      ; Copy count to COUNT2
        
        LDA TEXT        ; Load first text character
        STA TEXT1       ; Store in TEXT1
        LDA TEXT+1      ; Load second text character
        STA TEXT2       ; Store in TEXT2
        LDA TEXT+2      ; Load third text character
        STA TEXT3       ; Store in TEXT3
        
        LDA COUNT       ; Load text count
        SUB ONE         ; Subtract 1
        STA COUNT       ; Store updated count
        
        LDA TEXT1       ; Load first text character
        SUB PATTERN1    ; Compare with pattern character 1
        BRZ MATCH1      ; If match, go to MATCH1
        LDA TEXT2       ; Load second text character
        SUB PATTERN2    ; Compare with pattern character 2
        BRZ MATCH2      ; If match, go to MATCH2
        LDA TEXT3       ; Load third text character
        SUB PATTERN3    ; Compare with pattern character 3
        BRZ MATCH3      ; If match, go to MATCH3
        
        LDA COUNT       ; Load remaining count
        BRZ END         ; If count is zero, end
        LDA COUNT       ; Load count
        SUB ONE         ; Subtract 1
        STA COUNT       ; Store updated count
        LDA TEXT+3      ; Load next text character
        STA TEXT1       ; Shift text characters
        LDA TEXT+4      ; Load next text character
        STA TEXT2       ; Shift text characters
        LDA TEXT+5      ; Load next text character
        STA TEXT3       ; Shift text characters
        BRA LOOP        ; Go back to loop
        
MATCH1  LDA TEXT2       ; Load second text character
        SUB PATTERN2    ; Compare with pattern character 2
        BRZ MATCH2      ; If match, go to MATCH2
        BRA LOOP        ; Go back to loop
        
MATCH2  LDA TEXT3       ; Load third text character
        SUB PATTERN3    ; Compare with pattern character 3
        BRZ FOUND       ; If match, pattern found
        
MATCH3  LDA TEXT3       ; Load third text character
        SUB PATTERN3    ; Compare with pattern character 3
        BRZ FOUND       ; If match, pattern found
        
        BRA LOOP        ; Go back to loop
        
FOUND   LDA FOUND_FLAG  ; Load found flag
        STA OUTPUT      ; Store result
        BRA END         ; End program
        
LOOP    LDA COUNT       ; Load remaining count
        BRZ END         ; If zero, end
        BRA LOOP        ; Continue loop
        
END     HLT             ; Halt program

; Data Section
PATTERN1 DAT 0         ; Pattern character 1
PATTERN2 DAT 0         ; Pattern character 2
PATTERN3 DAT 0         ; Pattern character 3
TEXT1    DAT 0         ; Text character 1
TEXT2    DAT 0         ; Text character 2
TEXT3    DAT 0         ; Text character 3
COUNT    DAT 0         ; Text character count
COUNT2   DAT 0         ; Copy of count
FOUND_FLAG DAT 1      ; Flag indicating pattern found
OUTPUT   DAT 0         ; Output result
ONE      DAT 1         ; Constant 1
```

## How it works:

1. **Input**: The program takes a 3-character pattern and text to search through
2. **Initialization**: Sets up registers and loads pattern characters
3. **Comparison**: Compares pattern characters with text characters
4. **Shift**: When no match is found, shifts the text window forward
5. **Match Detection**: Uses branch instructions to detect character matches
6. **Output**: Sets a flag when pattern is found

## Key LMC Instructions Used:

- `INP` - Input data
- `STA` - Store accumulator
- `LDA` - Load accumulator
- `SUB` - Subtract
- `BRZ` - Branch if zero
- `BRA` - Branch always
- `HLT` - Halt program

This is a simplified version that demonstrates the core concepts of Boyer-Moore search in LMC's limited instruction set. A full implementation would require more sophisticated handling of bad character heuristics and good suffix rules.

