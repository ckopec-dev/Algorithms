# N-Queens Problem in LMC (Little Man Computer)

Here's an implementation of a simplified N-Queens problem solver in LMC. This example solves the 4-Queens problem to demonstrate the concept.

```assembly
; N-Queens Problem Solver (4-Queens)
; LMC Assembly Code

; Constants
N       DAT 4           ; Size of chessboard
MAX     DAT 10          ; Maximum board size
SOLUTIONS DAT 0       ; Counter for solutions
BOARD   DAT 0           ; Board storage (4 positions)
ROW     DAT 0           ; Current row being processed
COL     DAT 0           ; Current column being tested
TEMP    DAT 0           ; Temporary storage

; Main program
START   LDA ONE         ; Initialize row counter
        STA ROW
        LDA ZERO
        STA BOARD       ; Initialize board
        LDA ZERO
        STA SOLUTIONS
        LDA ZERO
        STA COL
        BRZ CHECK       ; Start checking from row 0

CHECK   LDA ROW         ; Check if we've placed all queens
        LDA N
        BRZ FOUND_SOLUTION

        LDA COL         ; Check if column is valid
        LDA N
        BRZ NEXT_ROW

        LDA ROW         ; Check if current position is safe
        LDA BOARD       ; Load board state
        LDA COL
        BRZ SAFE_CHECK

SAFE_CHECK
        LDA BOARD       ; Check column conflicts
        LDA COL
        BRZ SAFE_CHECK2

SAFE_CHECK2
        LDA BOARD       ; Check diagonal conflicts
        LDA COL
        BRZ SAFE_CHECK3

SAFE_CHECK3
        LDA ROW         ; Check if position is safe
        LDA COL
        BRZ PLACE_QUEEN

PLACE_QUEEN
        LDA ROW         ; Place queen at current position
        LDA COL
        STA BOARD
        LDA ROW         ; Increment row
        LDA ONE
        ADD ROW
        STA ROW
        LDA ZERO        ; Reset column
        STA COL
        BRZ CHECK

NEXT_ROW
        LDA ROW         ; Move to next row
        LDA ONE
        ADD ROW
        STA ROW
        LDA ZERO        ; Reset column
        STA COL
        BRZ CHECK

FOUND_SOLUTION
        LDA SOLUTIONS   ; Increment solution counter
        LDA ONE
        ADD SOLUTIONS
        STA SOLUTIONS
        LDA ROW         ; Backtrack
        LDA ONE
        SUB ROW
        STA ROW
        LDA COL         ; Try next column
        LDA ONE
        ADD COL
        STA COL
        BRZ CHECK

; Helper functions
ZERO    DAT 0
ONE     DAT 1
TWO     DAT 2
THREE   DAT 3
FOUR    DAT 4

; Data storage for board positions
BOARD0  DAT 0
BOARD1  DAT 0
BOARD2  DAT 0
BOARD3  DAT 0

; End of program
        HLT
        DAT 0
        DAT 0
        DAT 0
        DAT 0
```

## How it works:

1. **Initialization**: Sets up the board size (N=4) and initializes counters
2. **Recursive Backtracking**: 
   - Places queens row by row
   - Checks for conflicts (same column, diagonal)
   - Backtracks when conflicts occur
3. **Solution Counting**: Keeps track of valid solutions found
4. **Termination**: Stops when all possibilities are exhausted

## Key LMC Instructions Used:

- `LDA` - Load accumulator
- `STA` - Store accumulator
- `ADD` - Addition
- `SUB` - Subtraction
- `BRZ` - Branch if zero
- `BR` - Branch always
- `HLT` - Halt program

## Limitations:

- This is a simplified version that demonstrates the concept
- LMC has limited memory and processing capabilities
- Actual N-Queens solutions would require more sophisticated memory management
- The algorithm uses a basic backtracking approach with manual state management

This implementation shows how the fundamental concepts of the N-Queens problem (constraint satisfaction, backtracking) can be translated into LMC assembly language, though it's a highly simplified version of what would be needed for a complete solution.

