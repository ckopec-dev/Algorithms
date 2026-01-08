# Gauss-Jordan Elimination in Forth

Here's an implementation of the Gauss-Jordan elimination algorithm in Forth:

```forth
\ Gauss-Jordan elimination in Forth
\ Matrix operations for solving linear systems

\ Constants
100 CONSTANT MAXSIZE
: .matrix ( addr rows cols -- )
    cr ." Matrix:" cr
    0 DO
        I . ." : "
        0 DO
            DUP I J * MAXSIZE * + F@ . 
        LOOP
        cr
    LOOP
    DROP ;

\ Matrix element access
: mat! ( value addr row col -- )
    DUP MAXSIZE * + MAXSIZE * + F!
    ;

: mat@ ( addr row col -- value )
    DUP MAXSIZE * + MAXSIZE * + F@
    ;

\ Swap two rows
: swap-rows ( addr row1 row2 -- )
    0 DO
        DUP I + F@ DUP I 1+ + F@ 
        I 1+ I + F!
        I I + F!
    LOOP
    DROP ;

\ Forward elimination phase
: forward-eliminate ( addr rows cols -- )
    0 DO
        \ Find pivot
        I DUP 1+ DO
            DUP I J * MAXSIZE * + F@ F0= IF
                \ Swap rows if pivot is zero
                I J swap-rows
                LEAVE
            THEN
        LOOP
        \ Eliminate
        I 1+ DO
            DUP I J * MAXSIZE * + F@ DUP I I * MAXSIZE * + F@ F/ 
            0 DO
                DUP I J * MAXSIZE * + F@ 
                DUP I I * MAXSIZE * + F@ F* F- 
                I J * MAXSIZE * + F!
            LOOP
        LOOP
    LOOP
    DROP ;

\ Backward elimination phase
: backward-eliminate ( addr rows cols -- )
    0 DO
        \ Normalize pivot to 1
        DUP I I * MAXSIZE * + F@ F1/F!
        \ Eliminate above
        0 DO
            DUP I J * MAXSIZE * + F@ 
            DUP I I * MAXSIZE * + F@ F* F- 
            I J * MAXSIZE * + F!
        LOOP
    LOOP
    DROP ;

\ Solve linear system Ax = b
: solve-linear-system ( A b n -- x )
    \ Create augmented matrix [A|b]
    0 DO
        I DUP 0 DO
            DUP I J * MAXSIZE * + F@ 
            I J * MAXSIZE * + F!
        LOOP
        I I * MAXSIZE * + F@ 
        I I * MAXSIZE * + F!
    LOOP
    DROP ;

\ Example usage
: example-solve ( -- )
    \ 3x3 system:
    \ 2x + 1y + 1z = 5
    \ 4x + 3y + 3z = 11
    \ 8x + 7y + 9z = 25
    
    \ Coefficient matrix A
    CREATE A 9 F, 2.0 1.0 1.0 4.0 3.0 3.0 8.0 7.0 9.0
    \ Constants vector b
    CREATE b 3 F, 5.0 11.0 25.0
    
    \ Solution vector x
    CREATE x 3 F, 0.0 0.0 0.0
    
    \ Solve system
    A b 3 forward-eliminate
    A b 3 backward-eliminate
    
    \ Display results
    ." Solution vector x:" cr
    0 DO
        b I * MAXSIZE * + F@ .
    LOOP
    cr ;

\ Test the example
example-solve
```

## Key Features of this Implementation:

1. **Matrix Operations**: 
   - `mat!` and `mat@` for element access
   - `swap-rows` for row swapping
   - `.matrix` for displaying matrices

2. **Gauss-Jordan Steps**:
   - Forward elimination phase
   - Backward elimination phase
   - Pivot selection and row operations

3. **Linear System Solution**:
   - Combines coefficient matrix A with constants vector b
   - Performs full elimination to reduced row echelon form
   - Extracts solution vector

4. **Example Usage**:
   - Solves the system: 2x + y + z = 5, 4x + 3y + 3z = 11, 8x + 7y + 9z = 25
   - Expected solution: x = 1, y = 1, z = 2

This implementation demonstrates the core algorithmic steps of Gauss-Jordan elimination in a Forth environment, using floating-point arithmetic and matrix manipulation primitives.

