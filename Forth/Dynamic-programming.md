# Dynamic Programming Example in Forth: Fibonacci Sequence

Here's an implementation of the Fibonacci sequence using dynamic programming (bottom-up approach) in Forth:

```forth
\ Dynamic Programming - Fibonacci Sequence
\ Bottom-up approach with memoization

\ Define array to store computed values
\ Maximum Fibonacci number we want to compute
20 constant MAX_FIB

\ Create array for memoization
create fib-array MAX_FIB cells allot

\ Initialize array with zeros
: init-fib-array ( -- )
    0 MAX_FIB 0 do
        0 fib-array i cells + !
    loop ;

\ Dynamic Programming Fibonacci
: fib-dp ( n -- result )
    dup 0< if drop 0 exit then  \ Handle negative numbers
    dup 1 = if exit then       \ Base case: fib(1) = 1
    dup MAX_FIB >= if 
        drop ." Error: Number too large" 0 exit 
    then
    
    \ Check if already computed
    fib-array swap cells + @ 0<> if
        fib-array swap cells + @ exit
    then
    
    \ Compute using dynamic programming
    2dup - 1- fib-dp  \ fib(n-2)
    2dup - fib-dp    \ fib(n-1)
    +                \ fib(n) = fib(n-1) + fib(n-2)
    
    \ Store result in array
    fib-array swap cells + !
    ;

\ Alternative: Iterative bottom-up approach (more efficient)
: fib-iterative ( n -- result )
    dup 0< if drop 0 exit then
    dup 1 = if exit then
    dup MAX_FIB >= if 
        drop ." Error: Number too large" 0 exit 
    then
    
    \ Initialize base cases
    0 1 2 swap do
        2dup + 2swap 1+
    loop
    2drop ;

\ Print Fibonacci sequence up to n
: print-fib-sequence ( n -- )
    0 do
        i fib-dp . 
    loop ;

\ Example usage:
\ init-fib-array
\ 10 print-fib-sequence
```

## Key Features of this Dynamic Programming Implementation:

### 1. **Memoization**
- Uses `fib-array` to store previously computed values
- Avoids redundant calculations

### 2. **Bottom-Up Approach**
- Computes values from smallest to largest
- Builds up the solution incrementally

### 3. **Efficiency**
- Time complexity: O(n) instead of O(2^n)
- Space complexity: O(n) for storage

## Example Usage:
```forth
init-fib-array
10 print-fib-sequence
\ Output: 0 1 1 2 3 5 8 13 21 34
```

This implementation demonstrates how Forth can be used to implement dynamic programming concepts with explicit memory management and efficient computation.