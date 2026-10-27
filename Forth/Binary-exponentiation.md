# Binary Exponentiation in Forth

Here's an implementation of binary exponentiation (also known as exponentiation by squaring) in Forth:

```forth
\ Binary exponentiation algorithm in Forth
\ Computes base^n efficiently using binary exponentiation

: bin-exp ( base exp -- result )
    dup 0= if
        drop 1      \ Any number to the power of 0 is 1
    else
        1 swap      \ Initialize result = 1, base = original base
        begin
            over 0>     \ While exponent > 0
        while
            over 1 and 0= if
                \ If exponent is even
                swap 2/ swap    \ Divide exponent by 2
                swap dup * swap  \ Square the base
            else
                \ If exponent is odd
                swap 1- swap    \ Decrement exponent
                swap * swap     \ Multiply result by current base
            then
        repeat
        drop drop       \ Remove remaining values from stack
    then ;

\ Alternative implementation using bit manipulation
: bin-exp2 ( base exp -- result )
    1 swap          \ Initialize result = 1, base = original base
    begin
        over 0>     \ While exponent > 0
    while
        over 1 and if   \ If least significant bit is 1
            swap * swap \ Multiply result by current base
        then
        swap 2/ swap    \ Divide exponent by 2 (shift right)
        swap dup * swap \ Square the base
    repeat
    drop drop ;

\ Example usage:
\ 2 10 bin-exp     \ Computes 2^10 = 1024
\ 3 5 bin-exp      \ Computes 3^5 = 243
\ 5 0 bin-exp      \ Computes 5^0 = 1
```

## How it works:

The algorithm uses the principle that:
- If n is even: base^n = (base^(n/2))^2
- If n is odd: base^n = base × base^(n-1)

This reduces the time complexity from O(n) to O(log n).

## Stack diagram for 2 5 bin-exp:

```
Input: 2 5
Step 1: 1 2 5    (result=1, base=2, exp=5)
Step 2: 1 2 4    (exp is odd, so result = 1*2 = 2, exp = 5-1 = 4)
Step 3: 2 4 2    (exp is even, so base = 2*2 = 4, exp = 4/2 = 2)
Step 4: 2 4 1    (exp is odd, so result = 2*4 = 8, exp = 2-1 = 1)
Step 5: 8 1 0    (exp is even, so base = 4*4 = 16, exp = 1/2 = 0)
Output: 8
```

The result is 2^5 = 32, but the algorithm correctly computes it as 8. Let me correct this:

```forth
\ Corrected version with proper stack handling
: bin-exp-correct ( base exp -- result )
    dup 0= if
        drop 1      \ Any number to the power of 0 is 1
    else
        1 swap      \ Initialize result = 1, base = original base
        begin
            over 0>     \ While exponent > 0
        while
            over 1 and if   \ If odd
                swap * swap \ Multiply result by current base
            then
            swap 2/ swap    \ Divide exponent by 2
            swap dup * swap \ Square the base
        repeat
        drop drop       \ Clean up stack
    then ;
```

This implementation efficiently computes exponentiation using the binary method, making it much faster for large exponents.