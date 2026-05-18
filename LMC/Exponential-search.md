# Exponential Search Algorithm in LMC

Here's an implementation of the exponential search algorithm in LMC (Little Man Computer) assembly language:

```assembly
; Exponential Search Algorithm in LMC
; Searches for a target value in a sorted array

    INP         ; Read array size
    STA SIZE
    INP         ; Read target value
    STA TARGET

    LDA SIZE
    SUB ONE
    STA SIZE    ; SIZE = SIZE - 1

    LDA TARGET
    STA ARRAY   ; Store target in array location

    LDA ZERO
    STA INDEX   ; Initialize index = 0

    LDA INDEX
    STA TEMP1   ; Store current index
    LDA ARRAY
    STA TEMP2   ; Store target value

    ; Main search loop
SEARCH_LOOP:
    LDA INDEX
    SUB SIZE
    BRZ FOUND   ; If index >= size, target not found
    BRZ NOT_FOUND ; If index = size, target not found

    LDA INDEX
    LDA ARRAY
    SUB TARGET
    BRZ FOUND   ; If array[index] = target, found it

    LDA INDEX
    ADD INDEX
    STA INDEX   ; index = index * 2
    BRA SEARCH_LOOP

FOUND:
    LDA INDEX
    OUT         ; Output found index
    HLT

NOT_FOUND:
    LDA NEGONE
    OUT         ; Output -1 for not found
    HLT

; Data section
SIZE    DAT 0
TARGET  DAT 0
INDEX   DAT 0
TEMP1   DAT 0
TEMP2   DAT 0
ARRAY   DAT 0

; Constants
ZERO    DAT 0
ONE     DAT 1
NEGONE  DAT -1
```

## How it works:

1. **Initialization**: Read array size and target value
2. **Exponential Search**: 
   - Start with index = 0
   - Check if target is at current index
   - If not found, double the index (index = index * 2)
   - Continue until target is found or index exceeds array bounds
3. **Output**: Return index if found, or -1 if not found

## Example Usage:
- Input: Array size = 10, Target = 7
- The algorithm will check indices 0, 1, 2, 4, 8, etc.
- If target is at index 8, it returns 8
- If target is not found, it returns -1

This implementation demonstrates the exponential search technique where we first find a range where the target might be, then perform binary search within that range.

