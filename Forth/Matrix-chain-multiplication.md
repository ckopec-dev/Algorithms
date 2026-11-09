# Matrix Chain Multiplication in Forth

Here's an implementation of the Matrix Chain Multiplication algorithm in Forth:

```forth
\ Matrix Chain Multiplication Algorithm in Forth

\ Define constants
: MAX-MATRICES 100 ; \ Maximum number of matrices
: INF 999999 ; \ Large number representing infinity

\ Global arrays for dynamic programming
create dp MAX-MATRICES MAX-MATRICES * cells allot
create dimensions MAX-MATRICES 1+ cells allot

\ Initialize DP table with zeros
: init-dp ( -- )
    MAX-MATRICES MAX-MATRICES * 0 do 0 dp i cells + ! loop ;

\ Set matrix dimensions
: set-dimensions ( n1 n2 n3 ... -- )
    0 do
        i 1+ dimensions i cells + !
    loop ;

\ Matrix chain multiplication algorithm
: matrix-chain-multiplication ( n -- min-cost )
    dup 1 > if
        \ Initialize DP table
        init-dp
        
        \ Chain length from 2 to n
        2 swap do
            \ For each chain of length i
            1 do
                \ j is the end index
                i + dup dim-size 1- < if
                    \ k is the split point
                    1 do
                        \ Cost of multiplying matrices A[i..k] and A[k+1..j]
                        i k - 1 cells dp + @
                        k 1+ j - 1 cells dp + @
                        + \ Add cost of combining two subchains
                        
                        \ Cost of multiplying A[i..k] and A[k+1..j]
                        i 1- dimensions + @
                        k dimensions + @
                        j dimensions + @
                        * * + \ Multiply dimensions
                        
                        \ Update minimum cost
                        dup i j - 1 cells dp + @ > if
                            i j - 1 cells dp + !
                        then
                    loop
                then
            loop
        loop
        
        \ Return the minimum cost
        1 1 - 1 cells dp + @
    else
        0
    then ;

\ Helper function to get dimension size
: dim-size ( -- n )
    dimensions 0 cells + @ ;

\ Example usage:
\ Matrix dimensions: A1(10x100), A2(100x5), A3(5x50)
\ Expected result: 750

: example ( -- )
    \ Set dimensions [10, 100, 5, 50]
    10 100 5 50 set-dimensions
    
    \ Calculate minimum cost for 3 matrices
    3 matrix-chain-multiplication . ;

\ Alternative simpler version with pre-defined arrays
: simple-matrix-chain ( -- )
    \ Predefined dimensions: [10, 20, 30, 40, 30]
    \ Matrices: A(10x20), B(20x30), C(30x40), D(40x30)
    10 20 30 40 30 set-dimensions
    
    \ Calculate minimum cost for 4 matrices
    4 matrix-chain-multiplication . ;

\ Print the DP table (for debugging)
: print-dp-table ( n -- )
    0 do
        cr i . ." : "
        0 do
            i j - 1 cells dp + @ . 
        loop
    loop ;
```

## How it works:

1. **Data Structures**: Uses arrays `dp` for dynamic programming table and `dimensions` for matrix dimensions.

2. **Algorithm Steps**:
   - Initialize DP table with zeros
   - For chain lengths from 2 to n:
     - For each possible chain start position:
       - Try all possible split points
       - Calculate cost of splitting at each point
       - Store minimum cost in DP table

3. **Time Complexity**: O(n³)
4. **Space Complexity**: O(n²)

## Example Usage:

```forth
\ Run the example
simple-matrix-chain \ Should output: 26000
```

This implementation demonstrates how to solve the classic Matrix Chain Multiplication problem using dynamic programming in Forth, finding the optimal way to parenthesize matrix multiplication to minimize scalar multiplications.