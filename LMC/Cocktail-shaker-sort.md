# Cocktail Shaker Sort in LMC

Here's an implementation of the Cocktail Shaker Sort algorithm in LMC (Little Man Computer) assembly language:

```assembly
; Cocktail Shaker Sort Algorithm
; Sorts an array of 10 integers in ascending order

    INP         ; Read first number
    STA NUMBERS+0
    INP         ; Read second number
    STA NUMBERS+1
    INP         ; Read third number
    STA NUMBERS+2
    INP         ; Read fourth number
    STA NUMBERS+3
    INP         ; Read fifth number
    STA NUMBERS+4
    INP         ; Read sixth number
    STA NUMBERS+5
    INP         ; Read seventh number
    STA NUMBERS+6
    INP         ; Read eighth number
    STA NUMBERS+7
    INP         ; Read ninth number
    STA NUMBERS+8
    INP         ; Read tenth number
    STA NUMBERS+9

    LDA COUNT   ; Initialize count to 10
    STA COUNT1
    STA COUNT2
    LDA ZERO    ; Initialize LEFT to 0
    STA LEFT
    LDA TEN     ; Initialize RIGHT to 10
    STA RIGHT
    LDA ONE     ; Initialize SWAP to 1
    STA SWAP

SORT_LOOP:
    LDA SWAP    ; If SWAP = 0, sorting complete
    BRZ DONE

    LDA LEFT    ; Set current position to LEFT
    STA POS
    LDA ZERO    ; Reset SWAP to 0
    STA SWAP

    ; Forward pass (left to right)
FORWARD_PASS:
    LDA POS     ; Get current position
    LDA NUMBERS+0 ; Load first element
    ADD POS     ; Add position to get element address
    LDA NUMBERS+0 ; Load element
    STA TEMP1

    LDA POS     ; Get current position
    LDA ONE     ; Add 1 to get next position
    ADD POS
    LDA NUMBERS+0 ; Load next element
    ADD POS
    LDA NUMBERS+0 ; Load element
    STA TEMP2

    LDA TEMP1   ; Compare elements
    SUB TEMP2
    BRP NO_SWAP1 ; If temp1 >= temp2, no swap needed

    ; Swap elements
    LDA POS     ; Get current position
    LDA ONE     ; Add 1 to get next position
    ADD POS
    LDA NUMBERS+0 ; Load next element
    ADD POS
    LDA NUMBERS+0 ; Load element
    STA TEMP3

    LDA POS     ; Get current position
    LDA NUMBERS+0 ; Load element
    ADD POS
    LDA NUMBERS+0 ; Load element
    STA TEMP4

    LDA POS     ; Get current position
    LDA ONE     ; Add 1 to get next position
    ADD POS
    LDA NUMBERS+0 ; Load next element
    ADD POS
    LDA NUMBERS+0 ; Load element
    STA TEMP5

    ; Perform actual swap
    LDA POS     ; Get current position
    LDA ONE     ; Add 1 to get next position
    ADD POS
    LDA NUMBERS+0 ; Load next element
    ADD POS
    LDA NUMBERS+0 ; Load element
    STA TEMP6

    LDA POS     ; Get current position
    LDA NUMBERS+0 ; Load element
    ADD POS
    LDA NUMBERS+0 ; Load element
    STA TEMP7

    LDA ONE     ; Set SWAP to 1 (elements were swapped)
    STA SWAP

NO_SWAP1:
    LDA POS     ; Get current position
    LDA ONE     ; Add 1 to move to next element
    ADD POS
    STA POS

    LDA POS     ; Compare with RIGHT
    LDA RIGHT
    SUB POS
    BRP FORWARD_PASS ; Continue if not at right boundary

    LDA RIGHT   ; Move RIGHT boundary left
    LDA ONE
    SUB RIGHT
    STA RIGHT

    LDA RIGHT   ; Check if RIGHT <= LEFT
    LDA LEFT
    SUB RIGHT
    BRP DONE    ; If so, sorting complete

    LDA RIGHT   ; Set current position to RIGHT
    STA POS

    ; Backward pass (right to left)
BACKWARD_PASS:
    LDA POS     ; Get current position
    LDA ONE     ; Subtract 1 to get previous position
    SUB POS
    STA POS

    LDA POS     ; Get current position
    LDA NUMBERS+0 ; Load element
    ADD POS
    LDA NUMBERS+0 ; Load element
    STA TEMP1

    LDA POS     ; Get current position
    LDA ONE     ; Subtract 1 to get previous position
    SUB POS
    LDA NUMBERS+0 ; Load previous element
    ADD POS
    LDA NUMBERS+0 ; Load element
    STA TEMP2

    LDA TEMP2   ; Compare elements
    SUB TEMP1
    BRP NO_SWAP2 ; If temp2 >= temp1, no swap needed

    ; Swap elements
    LDA POS     ; Get current position
    LDA NUMBERS+0 ; Load element
    ADD POS
    LDA NUMBERS+0 ; Load element
    STA TEMP3

    LDA POS     ; Get current position
    LDA ONE     ; Subtract 1 to get previous position
    SUB POS
    LDA NUMBERS+0 ; Load previous element
    ADD POS
    LDA NUMBERS+0 ; Load element
    STA TEMP4

    LDA ONE     ; Set SWAP to 1 (elements were swapped)
    STA SWAP

NO_SWAP2:
    LDA POS     ; Get current position
    LDA LEFT    ; Compare with LEFT
    SUB POS
    BRP BACKWARD_PASS ; Continue if not at left boundary

    LDA LEFT    ; Move LEFT boundary right
    LDA ONE
    ADD LEFT
    STA LEFT

    LDA LEFT    ; Check if LEFT >= RIGHT
    LDA RIGHT
    SUB LEFT
    BRP DONE    ; If so, sorting complete

    JMP SORT_LOOP ; Continue sorting

DONE:
    LDA NUMBERS+0 ; Output sorted array
    OUT
    LDA NUMBERS+1
    OUT
    LDA NUMBERS+2
    OUT
    LDA NUMBERS+3
    OUT
    LDA NUMBERS+4
    OUT
    LDA NUMBERS+5
    OUT
    LDA NUMBERS+6
    OUT
    LDA NUMBERS+7
    OUT
    LDA NUMBERS+8
    OUT
    LDA NUMBERS+9
    OUT

    HLT         ; Halt program

; Data section
NUMBERS:    DAT 0     ; Array to store 10 numbers
            DAT 0
            DAT 0
            DAT 0
            DAT 0
            DAT 0
            DAT 0
            DAT 0
            DAT 0
            DAT 0

COUNT:      DAT 10    ; Array size
LEFT:       DAT 0     ; Left boundary
RIGHT:      DAT 10    ; Right boundary
SWAP:       DAT 0     ; Flag for swaps
POS:        DAT 0     ; Current position
TEMP1:      DAT 0     ; Temporary storage
TEMP2:      DAT 0     ; Temporary storage
TEMP3:      DAT 0     ; Temporary storage
TEMP4:      DAT 0     ; Temporary storage
TEMP5:      DAT 0     ; Temporary storage
TEMP6:      DAT 0     ; Temporary storage
TEMP7:      DAT 0     ; Temporary storage
COUNT1:     DAT 0     ; Counter
COUNT2:     DAT 0     ; Counter
ZERO:       DAT 0     ; Zero constant
ONE:        DAT 1     ; One constant
TEN:        DAT 10    ; Ten constant

```

## How it works:

1. **Input**: The program reads 10 integers from input and stores them in an array
2. **Initialization**: Sets up boundaries (LEFT=0, RIGHT=10) and a swap flag
3. **Sorting Loop**: 
   - Performs forward pass (left to right) comparing adjacent elements
   - Swaps elements if they're in wrong order
   - Updates the right boundary after forward pass
   - Performs backward pass (right to left) 
   - Updates the left boundary after backward pass
4. **Termination**: When no swaps occur, the array is sorted

## Key Features:

- **Bidirectional**: Alternates between forward and backward passes
- **Early Termination**: Stops when no swaps occur in a complete pass
- **Boundary Management**: Adjusts search boundaries after each pass
- **Simple Operations**: Uses only basic LMC instructions (ADD, SUB, BRZ, BRP, etc.)

This implementation demonstrates the core logic of Cocktail Shaker Sort in the constrained environment of LMC assembly language.

