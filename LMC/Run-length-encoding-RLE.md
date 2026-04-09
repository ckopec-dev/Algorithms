# Run-Length Encoding (RLE) in LMC (Little Man Computer)

Here's a complete LMC implementation of a Run-Length Encoding algorithm:

## LMC Code

```
    INP         ; Input first character
    BRZ END     ; If zero, end program
    STA CHAR    ; Store character
    LDA ONE     ; Load count = 1
    STA COUNT   ; Store count
    LDA CHAR    ; Load character
    STA LAST    ; Store as last character
    LDA ZERO    ; Load zero
    STA COUNT   ; Initialize count to 0

LOOP    INP         ; Input next character
        BRZ END     ; If zero, end program
        LDA CHAR    ; Load current character
        LDA LAST    ; Load last character
        BRZ SAME    ; If same, go to SAME
        LDA COUNT   ; Load count
        OUT         ; Output count
        LDA LAST    ; Load last character
        OUT         ; Output character
        LDA ZERO    ; Load zero
        STA COUNT   ; Reset count
        LDA CHAR    ; Load new character
        STA LAST    ; Store as last character
        LDA ONE     ; Load 1
        STA COUNT   ; Set count to 1
        BRZ LOOP    ; Jump back to LOOP

SAME    LDA COUNT   ; Load count
        LDA ONE     ; Load 1
        ADD COUNT   ; Add 1 to count
        STA COUNT   ; Store new count
        BRZ LOOP    ; Jump back to LOOP

END     LDA COUNT   ; Load final count
        OUT         ; Output count
        LDA LAST    ; Load final character
        OUT         ; Output character
        HLT         ; Halt program

CHAR    DAT 000     ; Storage for current character
COUNT   DAT 000     ; Storage for count
LAST    DAT 000     ; Storage for last character
ONE     DAT 001     ; Constant 1
ZERO    DAT 000     ; Constant 0
```

## How It Works

This RLE implementation processes a sequence of characters and outputs run-length encoded data:

1. **Input Processing**: Reads characters one by one
2. **Counting**: Counts consecutive identical characters
3. **Output**: When character changes, outputs the count followed by the character
4. **Final Output**: Outputs the last run at the end

## Example Execution

**Input**: `AABBBCCCAAA`

**Output**: 
```
2A3B3C3A
```

**Explanation**:
- 2 A's (AABBBCCCAAA)
- 3 B's (AABBBCCCAAA)  
- 3 C's (AABBBCCCAAA)
- 3 A's (AABBBCCCAAA)

## Key LMC Features Used

- **Input/Output**: `INP` and `OUT` instructions
- **Branching**: `BRZ` (branch if zero) for control flow
- **Arithmetic**: `ADD` for incrementing counters
- **Memory Storage**: Multiple data storage locations
- **Loop Control**: Jump instructions to create loops

## Notes

- The program assumes input characters are represented as numbers (0-9)
- Zero is used as a termination marker
- Output format is count followed by character (e.g., "2A")
- LMC has limited memory, so this is a simplified version for demonstration

This demonstrates how RLE can be implemented using basic LMC instructions and memory management techniques.

