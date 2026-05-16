# Binary Search in LMC (Little Man Computer)

Here's an implementation of binary search algorithm in LMC assembly language:

```assembly
; Binary Search Algorithm in LMC
; Searches for a value in a sorted array

    INP         ; Input the target value to search for
    STA TARGET  ; Store target value
    LDA COUNT   ; Load array size
    STA HIGH    ; Initialize high = count - 1
    LDA HIGH
    SUB ONE
    STA HIGH    ; HIGH = COUNT - 1

    LDA ZERO    ; Initialize low = 0
    STA LOW

LOOP    LDA LOW     ; Load low
    SUB HIGH    ; Compare low with high
    BRZ FOUND   ; If low = high, found it
    BRZ NOT_FOUND ; If low > high, not found
    BRZ NOT_FOUND ; If low > high, not found

    LDA LOW     ; Calculate mid = (low + high) / 2
    ADD HIGH
    DIV TWO
    STA MID

    LDA MID     ; Load array[mid]
    LDA ARRAY   ; Array address
    ADD MID
    LDA ARRAY   ; Load value at array[mid]
    STA MIDDLE  ; Store middle value

    LDA TARGET  ; Compare target with middle
    SUB MIDDLE
    BRZ FOUND   ; If equal, found it

    LDA TARGET  ; If target < middle
    SUB MIDDLE
    BRP HIGH_HALF ; If target > middle, search upper half

    ; Search lower half
    LDA MID     ; mid - 1
    SUB ONE
    STA HIGH
    BRA LOOP

HIGH_HALF LDA MID     ; mid + 1
    ADD ONE
    STA LOW
    BRA LOOP

FOUND   LDA MID     ; Found at position mid
    OUT         ; Output position
    HLT

NOT_FOUND LDA ZERO    ; Not found
    OUT         ; Output 0
    HLT

; Data section
TARGET  DAT 000     ; Target value to search for
LOW     DAT 000     ; Low pointer
HIGH    DAT 000     ; High pointer
MID     DAT 000     ; Mid pointer
MIDDLE  DAT 000     ; Value at middle position
COUNT   DAT 005     ; Array size (5 elements)
ARRAY   DAT 001     ; Array elements: 1, 3, 5, 7, 9
        DAT 003
        DAT 005
        DAT 007
        DAT 009
ZERO    DAT 000     ; Constant 0
ONE     DAT 001     ; Constant 1
TWO     DAT 002     ; Constant 2
```

## How it works:

1. **Input**: The program first inputs the target value to search for
2. **Initialization**: Sets up low=0 and high=COUNT-1
3. **Loop**: 
   - Calculates mid point using (low + high) / 2
   - Compares target with middle element
   - If found, outputs the position
   - If target is smaller, searches lower half
   - If target is larger, searches upper half
4. **Termination**: Either outputs found position or 0 if not found

## Example execution:
- Array: [1, 3, 5, 7, 9] (size = 5)
- Search for target = 5
- Expected output: 2 (position of 5 in array)

This implementation demonstrates the classic binary search algorithm with proper bounds checking and efficient logarithmic time complexity.

