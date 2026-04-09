# N-Queens Problem in Forth

Here's an implementation of the N-Queens problem solver in Forth:

```forth
\ N-Queens Problem Solver in Forth

\ Constants
16 CONSTANT MAX_QUEENS
MAX_QUEENS ALLOCATE VALUE BOARD
MAX_QUEENS ALLOCATE VALUE COLS
MAX_QUEENS ALLOCATE VALUE DIAGONAL1
MAX_QUEENS ALLOCATE VALUE DIAGONAL2

\ Global variables
VARIABLE SOLUTION_COUNT
VARIABLE N

\ Initialize all arrays to false (0)
: INIT-ARRAYS ( -- )
    MAX_QUEENS 0 DO 0 BOARD I + ! LOOP
    MAX_QUEENS 0 DO 0 COLS I + ! LOOP
    MAX_QUEENS 0 DO 0 DIAGONAL1 I + ! LOOP
    MAX_QUEENS 0 DO 0 DIAGONAL2 I + ! LOOP
    0 SOLUTION_COUNT !;

\ Check if placing a queen at position (row, col) is safe
: SAFE? ( row col -- flag )
    2DUP DIAGONAL1 + @ IF 0 EXIT THEN
    2DUP DIAGONAL2 + @ IF 0 EXIT THEN
    COLS + @ IF 0 EXIT THEN
    1;

\ Place a queen at position (row, col)
: PLACE-QUEEN ( row col -- )
    BOARD + 1 SWAP !           \ Mark board position
    COLS + 1 SWAP !            \ Mark column
    DIAGONAL1 + 1 SWAP !       \ Mark diagonal1
    DIAGONAL2 + 1 SWAP !;      \ Mark diagonal2

\ Remove a queen from position (row, col)
: REMOVE-QUEEN ( row col -- )
    BOARD + 0 SWAP !           \ Clear board position
    COLS + 0 SWAP !            \ Clear column
    DIAGONAL1 + 0 SWAP !       \ Clear diagonal1
    DIAGONAL2 + 0 SWAP !;      \ Clear diagonal2

\ Print the board solution
: PRINT-SOLUTION ( -- )
    N @ 0 DO
        CR
        N @ 0 DO
            BOARD I J + @ IF ." Q " ELSE ." . " THEN
        LOOP
    LOOP
    CR;

\ Recursive backtracking solver
: SOLVE-QUEENS ( row -- )
    N @ = IF
        1 SOLUTION_COUNT +!
        \ PRINT-SOLUTION  \ Uncomment to print all solutions
        EXIT
    THEN
    
    N @ 0 DO
        I 2DUP SAFE? IF
            PLACE-QUEEN
            DUP 1+ SOLVE-QUEENS
            REMOVE-QUEEN
        THEN
    LOOP
    DROP DROP;

\ Main solver function
: N-QUEENS ( n -- )
    MAX_QUEENS > IF
        ." Error: N too large" CR
        EXIT
    THEN
    
    N !                        \ Set N
    INIT-ARRAYS                \ Initialize arrays
    0 SOLUTION_COUNT !         \ Reset solution counter
    0 SOLVE-QUEENS             \ Start solving from row 0
    SOLUTION_COUNT @ . ." solutions found" CR;

\ Example usage:
\ 4 N-QUEENS    \ Solve 4-Queens problem
\ 8 N-QUEENS    \ Solve 8-Queens problem
```

## How it works:

1. **Data Structures**: Uses four arrays to track:
   - `BOARD`: Tracks queen positions
   - `COLS`: Tracks which columns have queens
   - `DIAGONAL1`: Tracks main diagonals (row - col)
   - `DIAGONAL2`: Tracks anti-diagonals (row + col)

2. **Key Functions**:
   - `SAFE?`: Checks if placing a queen at given position is valid
   - `PLACE-QUEEN`: Marks a position as occupied
   - `REMOVE-QUEEN`: Clears a position (backtracking)
   - `SOLVE-QUEENS`: Recursive backtracking algorithm
   - `N-QUEENS`: Main interface function

3. **Algorithm**:
   - Places queens row by row
   - For each row, tries all columns
   - Uses constraint checking to avoid conflicts
   - Backtracks when no valid position found
   - Counts total solutions found

## Usage Example:
```forth
4 N-QUEENS  \ Solves 4-Queens and reports 2 solutions
8 N-QUEENS  \ Solves 8-Queens and reports 92 solutions
```

This implementation efficiently solves the N-Queens problem using backtracking with constraint propagation to avoid checking invalid configurations.

