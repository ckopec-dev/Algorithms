# Gauss-Jordan Elimination in Forth

Here's an implementation of the Gauss-Jordan elimination algorithm in Forth:

```forth
\ Gauss-Jordan Elimination Algorithm in Forth

\ Constants
100 CONSTANT MAXROWS
100 CONSTANT MAXCOLS

\ Matrix storage (row-major order)
: matrix ( rows cols -- )
    * 2* * ;  \ allocate space for doubles

\ Initialize matrix to zero
: init-matrix ( addr rows cols -- )
    >r 0 do 0. r@ i * + 0. f! loop r> drop ;

\ Get element from matrix
: m@ ( addr row col rows -- value )
    >r * r> + f@ ;

\ Set element in matrix
: m! ( value addr row col rows -- )
    >r * r> + f! ;

\ Print matrix
: print-matrix ( addr rows cols -- )
    >r 0 do
        cr 0 do
            r@ i j 2dup r> 1+ 1- m@
            8 4 fg f. space
        loop
        r> 1+ 1- drop
    loop ;

\ Swap two rows
: swap-rows ( addr row1 row2 rows -- )
    >r 2dup = if exit then
    0 do
        r@ i 2dup m@  \ Get element from row1
        r@ i 2dup 1+ m@  \ Get element from row2
        r@ i 2dup 1+ m!  \ Put row2's element in row1
        r@ i 2dup m!     \ Put row1's element in row2
    loop r> drop ;

\ Forward elimination phase
: forward-elimination ( addr rows cols -- )
    >r 0 do
        \ Find pivot element
        i 1+ r@ 2* 1+ 0 do
            r@ i j 1+ 2dup r> m@
            0. f< if 1 else 0 then  \ Check if negative
            if
                r> i j swap-rows
                leave
            then
        loop r> drop
        \ Eliminate column below pivot
        i 1+ do
            r@ i j m@  \ Get pivot element
            r@ i 1+ 2dup m@
            f/  \ Divide to get multiplier
            0 do
                r@ i j 2dup m@  \ Get current element
                r@ i 1+ 2dup m@  \ Get element from row below
                f*  \ Multiply by multiplier
                f-  \ Subtract from current element
                r@ i 1+ 2dup m!  \ Store result
            loop
        loop
    loop r> drop ;

\ Backward elimination phase
: backward-elimination ( addr rows cols -- )
    >r r@ 1- 0 do
        \ Normalize pivot row
        r@ i 2dup m@
        0. f= if
            \ Skip if zero pivot
            continue
        then
        0 do
            r@ i j 2dup m@ f/ r@ i j 2dup m!
        loop
        \ Eliminate column above pivot
        0 i do
            r@ i 1+ 2dup m@
            f*  \ Get multiplier
            0 do
                r@ i 1+ 2dup m@  \ Get element from row below
                r@ i j 2dup m@  \ Get element from current row
                f*  \ Multiply by multiplier
                f-  \ Subtract from current element
                r@ i j 2dup m!  \ Store result
            loop
        loop
    loop r> drop ;

\ Solve system Ax = b using Gauss-Jordan elimination
: gauss-jordan-solve ( A rows cols b -- x )
    >r 0 do
        r@ 1+ 1- 0 do
            r@ i j 2dup m@  \ Get element from A
            r@ i r> 1+ 2dup m!  \ Copy to augmented matrix
        loop r> drop
        \ Copy b vector to last column
        r@ i 1+ 2dup m!
    loop r> drop
    \ Perform elimination
    forward-elimination
    backward-elimination ;

\ Example usage:
\ Create a 3x3 matrix and solve system
: example ( -- )
    3 4 matrix  \ 3x4 augmented matrix [A|b]
    0 0 2. f!  \ A[0][0] = 2
    0 1 1. f!  \ A[0][1] = 1
    0 2 1. f!  \ A[0][2] = 1
    0 3 8. f!  \ b[0] = 8
    
    1 0 3. f!  \ A[1][0] = 3
    1 1 2. f!  \ A[1][1] = 2
    1 2 -1. f! \ A[1][2] = -1
    1 3 11. f! \ b[1] = 11
    
    2 0 -1. f! \ A[2][0] = -1
    2 1 1. f!  \ A[2][1] = 1
    2 2 2. f!  \ A[2][2] = 2
    2 3 3. f!  \ b[2] = 3
    
    cr ." Original system:" 
    3 4 print-matrix
    
    cr ." Solution vector x:" 
    3 4 gauss-jordan-solve ;

\ Run the example
example
```

## Key Features of This Implementation:

1. **Matrix Storage**: Uses a row-major array to store floating-point numbers
2. **Element Access**: `m@` and `m!` for getting/setting matrix elements
3. **Row Swapping**: `swap-rows` function to handle pivoting
4. **Forward Elimination**: Reduces matrix to row echelon form
5. **Backward Elimination**: Further reduces to reduced row echelon form
6. **Solution Extraction**: Returns the solution vector

## Example System Solved:
```
2x + y + z = 8
3x + 2y - z = 11
-x + y + 2z = 3
```

This implementation handles the complete Gauss-Jordan elimination process including partial pivoting for numerical stability.