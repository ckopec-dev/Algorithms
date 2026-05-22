# Ternary Search in LMC (Little Man Computer)

Here's an example of a ternary search algorithm implemented in LMC:

```assembly
; Ternary Search Algorithm in LMC
; Searches for a value in a sorted array using ternary search

    INP         ; Input array size
    STA SIZE
    INP         ; Input search value
    STA TARGET

    ; Initialize array elements (for demonstration)
    LDA #100
    STA ARRAY
    LDA #200
    STA ARRAY+1
    LDA #300
    STA ARRAY+2
    LDA #400
    STA ARRAY+3
    LDA #500
    STA ARRAY+4
    LDA #600
    STA ARRAY+5
    LDA #700
    STA ARRAY+6
    LDA #800
    STA ARRAY+7
    LDA #900
    STA ARRAY+8
    LDA #1000
    STA ARRAY+9

    LDA #0      ; Initialize left pointer
    STA LEFT
    LDA SIZE    ; Initialize right pointer
    STA RIGHT
    LDA #0      ; Initialize result
    STA RESULT

    ; Main ternary search loop
SEARCH_LOOP
    LDA LEFT    ; Calculate mid1 = left + (right-left)/3
    LDA RIGHT
    SUB LEFT
    STA TEMP1
    LDA TEMP1
    DIV #3
    STA MID1
    LDA LEFT
    ADD MID1
    STA MID1

    LDA LEFT    ; Calculate mid2 = left + 2*(right-left)/3
    LDA RIGHT
    SUB LEFT
    STA TEMP1
    LDA TEMP1
    MUL #2
    DIV #3
    STA MID2
    LDA LEFT
    ADD MID2
    STA MID2

    ; Compare with mid1
    LDA MID1
    LDA ARRAY   ; Load array[mid1]
    SUB TARGET
    BRZ FOUND   ; If equal, found it
    BRL GO_LEFT ; If target < array[mid1], go left

    ; Compare with mid2
    LDA MID2
    LDA ARRAY   ; Load array[mid2]
    SUB TARGET
    BRZ FOUND   ; If equal, found it
    BRL GO_RIGHT ; If target > array[mid2], go right

GO_LEFT
    LDA MID1
    SUB #1
    STA RIGHT   ; right = mid1 - 1
    BRA SEARCH_LOOP

GO_RIGHT
    LDA MID2
    ADD #1
    STA LEFT    ; left = mid2 + 1
    BRA SEARCH_LOOP

FOUND
    LDA MID1    ; Store found index
    STA RESULT
    BRA END

END
    LDA RESULT  ; Output result
    OUT
    HLT         ; Halt

; Data section
SIZE   DAT 10
TARGET DAT 0
LEFT   DAT 0
RIGHT  DAT 0
RESULT DAT 0
MID1   DAT 0
MID2   DAT 0
TEMP1  DAT 0
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
```

## How it works:

1. **Input**: The program takes the array size and target value as input
2. **Initialization**: Sets up the left and right pointers for the search range
3. **Ternary Search Logic**: 
   - Calculates two midpoints (mid1 and mid2) that divide the array into three parts
   - Compares the target with both midpoints
   - Narrows the search to one of three segments based on comparisons
4. **Loop**: Continues until the element is found or search space is exhausted
5. **Output**: Returns the index of the found element or indicates not found

## Key LMC Features Used:

- **Input/Output**: `INP` and `OUT` instructions for user interaction
- **Arithmetic**: `ADD`, `SUB`, `MUL`, `DIV` for calculations
- **Branching**: `BRZ` (branch if zero) and `BRL` (branch if less) for conditional logic
- **Memory**: Various data storage locations for variables and array elements

## Note:
This is a simplified implementation. A complete ternary search in LMC would require more sophisticated handling of array indexing and additional boundary checking for production use.

