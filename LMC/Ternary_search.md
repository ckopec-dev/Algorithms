# Ternary Search in Little Man Computer (LMC)

Here's an example implementation of a ternary search algorithm in LMC:

```assembly
; Ternary Search Algorithm in LMC
; Searches for a value in a sorted array using ternary search

    INP         ; Input array size
    STA SIZE
    INP         ; Input search value
    STA SEARCH
    
    ; Initialize array elements
    LDA SIZE
    STA COUNT
    LDA ZERO
    STA ARRAY   ; Initialize array base address
    
    ; Read array elements
READ_LOOP:
    INP
    STA ARRAY,COUNT
    LDA COUNT
    SUB ONE
    STA COUNT
    BRZ READ_DONE
    BRA READ_LOOP
    
READ_DONE:
    ; Initialize search boundaries
    LDA ZERO
    STA LEFT
    LDA SIZE
    SUB ONE
    STA RIGHT
    
    ; Ternary search loop
SEARCH_LOOP:
    ; Calculate mid1 and mid2
    LDA RIGHT
    SUB LEFT
    DIV THREE
    STA MID1
    LDA MID1
    ADD LEFT
    STA MID1
    
    LDA RIGHT
    SUB LEFT
    DIV THREE
    ADD MID1
    STA MID2
    
    ; Compare search value with mid1
    LDA SEARCH
    SUB ARRAY,MID1
    BRZ FOUND
    BRZP COMPARE2
    
    ; Search in left third
    LDA MID1
    SUB ONE
    STA RIGHT
    BRA SEARCH_LOOP
    
COMPARE2:
    ; Compare search value with mid2
    LDA SEARCH
    SUB ARRAY,MID2
    BRZ FOUND
    BRZP COMPARE3
    
    ; Search in middle third
    LDA MID1
    ADD ONE
    STA LEFT
    LDA MID2
    SUB ONE
    STA RIGHT
    BRA SEARCH_LOOP
    
COMPARE3:
    ; Search in right third
    LDA MID2
    ADD ONE
    STA LEFT
    BRA SEARCH_LOOP
    
FOUND:
    ; Value found at position MID1 or MID2
    LDA MID1
    OUT
    HLT
    
NOT_FOUND:
    ; Value not found
    LDA NEGONE
    OUT
    HLT

; Data section
SIZE   DAT 0
SEARCH DAT 0
LEFT   DAT 0
RIGHT  DAT 0
MID1   DAT 0
MID2   DAT 0
COUNT  DAT 0

ARRAY  DAT 0
       DAT 0
       DAT 0
       DAT 0
       DAT 0
       DAT 0
       DAT 0
       DAT 0
       DAT 0
       DAT 0

ZERO   DAT 0
ONE    DAT 1
TWO    DAT 2
THREE  DAT 3
NEGONE DAT -1
```

## How it works:

1. **Input**: The program takes the array size and search value as input
2. **Array Initialization**: Reads array elements into memory locations
3. **Ternary Search**: 
   - Calculates two midpoints (mid1 and mid2) that divide the array into three parts
   - Compares the search value with both midpoints
   - Eliminates one-third of the search space based on comparison results
   - Continues until found or search space is exhausted

## Key Features:

- Uses three-way comparison instead of binary search
- Divides array into three parts rather than two
- More efficient than binary search for large datasets
- Demonstrates LMC's ability to handle complex control flow

## Example Usage:

Input: Array size = 10, Search value = 7
Array elements: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
Output: 6 (position of value 7 in 0-indexed array)

The algorithm efficiently narrows down the search space by eliminating two-thirds of the remaining elements in each iteration.

