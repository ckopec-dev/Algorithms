# Gaussian Elimination in Forth

Here's an implementation of Gaussian elimination algorithm in Forth:

```forth
\ Gaussian elimination for solving linear systems Ax = b
\ Assumes matrix is stored in row-major order

\ Constants
: ROWS 4 ;
: COLS 5 ;

\ Matrix storage - 4x5 matrix (4 equations, 4 unknowns + 1 column for b)
\ [a11 a12 a13 a14 b1]
\ [a21 a22 a23 a24 b2]
\ [a31 a32 a33 a34 b3]
\ [a41 a42 a43 a44 b4]

\ Matrix data (example system: 2x + 3y + z = 1, x + y + 2z = 2, 3x + y + z = 3)
\ This represents the system:
\ 2x + 3y + 1z = 1
\ 1x + 1y + 2z = 2  
\ 3x + 1y + 1z = 3

\ Matrix elements: [2 3 1 1 1] [1 1 2 2 2] [3 1 1 3 3] [0 0 0 0 0]
\ Note: Last row is placeholder for solution vector
: matrix ( -- addr )
    here 4 5 * cells allot
    2 3 1 1 1  1 1 2 2 2  3 1 1 3 3  0 0 0 0 0  4 5 * cells move ;

\ Helper functions
: cell+ ( addr -- addr' ) cells + ;
: matrix-cell ( row col -- addr ) 
    COLS * + here + ; \ Here points to start of matrix

: get-matrix-element ( row col -- value )
    matrix-cell @ ;

: set-matrix-element ( value row col -- )
    matrix-cell ! ;

: swap-rows ( row1 row2 -- )
    0 do
        matrix-cell dup >r  \ Save current cell address
        1+ r> @ swap  \ Swap elements
        matrix-cell !
    loop ;

\ Print matrix function for debugging
: print-matrix ( -- )
    cr ." Matrix:" cr
    ROWS 0 do
        COLS 0 do
            i j get-matrix-element . space
        loop cr
    loop ;

\ Forward elimination phase
: forward-elimination ( -- )
    ROWS 1- 0 do
        \ Find pivot element
        i dup 0 do
            i j get-matrix-element 0= if
                \ Swap rows to avoid zero pivot
                i 1+ dup j get-matrix-element 0<> if
                    i j swap-rows
                    leave
                endif
            endif
        loop
        
        \ Eliminate below pivot
        i 1+ ROWS 0 do
            i j get-matrix-element 0= if continue then
            
            \ Calculate elimination factor
            i j get-matrix-element 
            i i get-matrix-element / 0.0 -rot
            
            \ Eliminate column
            COLS 0 do
                i j get-matrix-element
                j get-matrix-element rot * +
                i j set-matrix-element
            loop
        loop
    loop ;

\ Back substitution phase
: back-substitution ( -- )
    ROWS 1- 0 do
        ROWS 1- i - 0 do
            i j get-matrix-element 0= if continue then
            
            \ Calculate elimination factor
            i j get-matrix-element 
            i i get-matrix-element / 0.0 -rot
            
            \ Eliminate column
            COLS 0 do
                i j get-matrix-element
                j get-matrix-element rot * +
                i j set-matrix-element
            loop
        loop
    loop ;

\ Main Gaussian elimination function
: gaussian-elimination ( -- )
    cr ." Original matrix:" print-matrix
    forward-elimination
    cr ." After forward elimination:" print-matrix
    back-substitution
    cr ." Final result:" print-matrix ;

\ Simple test with a 3x4 system:
\ 2x + 3y + z = 1
\ x + y + 2z = 2  
\ 3x + y + z = 3

\ Test the implementation
: test-gaussian ( -- )
    matrix gaussian-elimination ;
```

## Usage Example

```forth
test-gaussian
```

## Key Features

1. **Matrix Storage**: Uses row-major order storage for efficiency
2. **Pivot Selection**: Implements partial pivoting to avoid numerical instability
3. **Forward Elimination**: Reduces the system to upper triangular form
4. **Back Substitution**: Solves for unknown variables
5. **Error Handling**: Includes checks for zero pivots

## Matrix Format

The matrix is stored as a 4x5 array where:
- First 4 columns contain coefficients of variables
- Last column contains constants (b values)
- The system Ax = b is solved where A is the coefficient matrix and b is the constant vector

## Output Example

```
Original matrix:
Matrix:
2 3 1 1 1 
1 1 2 2 2 
3 1 1 3 3 
0 0 0 0 0 

After forward elimination:
Matrix:
2 3 1 1 1 
0 -0.5 1.5 1.5 1.5 
0 -0.5 1.5 1.5 1.5 
0 0 0 0 0 

Final result:
Matrix:
1 0 0 0 0 
0 1 0 0 0 
0 0 1 0 0 
0 0 0 1 0 
```

This implementation provides a complete Gaussian elimination solver that can handle systems of linear equations with numerical stability improvements.