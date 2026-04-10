# Cholesky Decomposition in Forth

Here's an implementation of the Cholesky decomposition algorithm in Forth:

```forth
\ Cholesky Decomposition in Forth
\ Decomposes a symmetric positive definite matrix into L * L^T
\ where L is a lower triangular matrix

: cholesky ( addr n -- )
    \ addr: pointer to matrix, n: size of matrix
    over 0 do
        \ Compute diagonal elements
        i cells + 0 do
            \ Compute L[i,j] = (A[i,j] - sum(L[i,k]*L[j,k])) / L[j,j]
            i j < if
                \ Compute sum of products
                0.0e 0 do
                    i cells + k cells + @
                    j cells + k cells + @
                    * +
                loop
                \ Subtract from A[i,j]
                i cells + j cells + @
                swap -
                \ Divide by L[j,j]
                j cells + j cells + @
                / 
                \ Store result
                i cells + j cells + !
            else
                \ Diagonal element computation
                0.0e 0 do
                    i cells + k cells + @
                    i cells + k cells + @
                    * +
                loop
                \ Subtract from A[i,i]
                i cells + i cells + @
                swap -
                \ Take square root
                sqrt
                \ Store result
                i cells + i cells + !
            then
        loop
    loop
    \ Zero out upper triangular part
    0 do
        0 j do
            i cells + j cells + 0.0e !
        loop
    loop
;

\ Example usage:
\ Create a 3x3 matrix on the stack
\ Matrix A = [ 4.0 12.0 -16.0 ]
\          [ 12.0 37.0 -43.0 ]
\          [ -16.0 -43.0 98.0 ]

\ Stack: 12.0 37.0 98.0 12.0 37.0 4.0 3 0
\ Then call: 0 3 cholesky
\ Result will be stored in the same memory location
```

## Alternative Implementation with Better Stack Management

```forth
\ More readable Cholesky decomposition
\ Matrix stored in column-major order

: cholesky-decomp ( matrix n -- )
    \ matrix: pointer to matrix data, n: size of square matrix
    >r                    \ Save size on return stack
    r@ 0 do               \ For each row
        r@ 0 do           \ For each column
            i j < if      \ If not diagonal element
                \ Compute sum of products
                0.0e 0 do
                    i r@ * + cells +    \ L[i,k]
                    j r@ * + cells +    \ L[j,k]
                    * +                 \ Add to sum
                loop
                \ Compute L[i,j]
                i r@ * + j r@ * + cells + @
                swap -
                j r@ * + j r@ * + cells + @
                / 
                i r@ * + j r@ * + cells + !
            else          \ Diagonal element
                \ Compute sum of squares
                0.0e 0 do
                    i r@ * + cells + @
                    i r@ * + cells + @
                    * +
                loop
                \ Compute L[i,i]
                i r@ * + i r@ * + cells + @
                swap -
                sqrt
                i r@ * + i r@ * + cells + !
            then
        loop
    loop
    r> drop               \ Clean up return stack
;

\ Helper to zero upper triangular part
: zero-upper ( matrix n -- )
    >r
    0 do
        0 i do
            i r@ * + j r@ * + cells + 0.0e !
        loop
    loop
    r> drop
;
```

## Usage Example

```forth
\ Example: Decompose matrix A = 
\ [ 4.0 12.0 -16.0 ]
\ [ 12.0 37.0 -43.0 ]
\ [ -16.0 -43.0 98.0 ]

\ Store matrix in memory (column-major order)
\ 4.0 12.0 -16.0 12.0 37.0 -43.0 -16.0 -43.0 98.0

\ Call:
\ 0 3 cholesky-decomp
\ 0 3 zero-upper
```

This implementation assumes:
- Matrix is stored in column-major order
- Matrix is symmetric and positive definite
- Memory is allocated for the matrix
- Stack-based approach with proper memory management
- Uses floating-point arithmetic (requires floating-point Forth)

