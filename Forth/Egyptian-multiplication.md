# Egyptian Multiplication in Forth

Egyptian multiplication (also known as binary multiplication or Russian peasant multiplication) can be implemented in Forth as follows:

```forth
\ Egyptian multiplication algorithm
\ Multiplies two numbers using repeated doubling and addition

: egyptian-mul ( a b -- product )
    0 swap      \ Initialize result to 0, move b to top
    begin       \ Start loop
        dup 0>  \ Check if b > 0
    while
        dup 1 and 0<>  \ Check if b is odd
        if
            rot + rot  \ Add a to result, keep b on top
        else
            drop       \ Discard b if even
        then
        2/         \ Divide b by 2 (integer division)
        2*         \ Multiply a by 2
    repeat
    drop        \ Remove remaining b (which is now 0)
;

\ Example usage:
\ 13 11 egyptian-mul .  \ Prints: 143

\ Alternative implementation with clearer variable names
: egyptian-mul2 ( a b -- product )
    0 >r        \ Store result in return stack
    begin
        dup 0>  \ While b > 0
    while
        dup 1 and 0<>  \ If b is odd
        if
            r> + >r  \ Add a to result
        then
        2/         \ b = b/2
        2*         \ a = a*2
    repeat
    r> drop     \ Clean up return stack
;

\ Test examples
cr
." 6 × 7 = " 6 7 egyptian-mul .
cr
." 13 × 11 = " 13 11 egyptian-mul .
cr
." 25 × 16 = " 25 16 egyptian-mul .
```

## How it works:

1. **Initialize**: Start with result = 0
2. **Loop while b > 0**:
   - If b is odd: add a to result
   - Divide b by 2 (integer division)
   - Multiply a by 2
3. **Return**: When b becomes 0, return result

## Example trace for 13 × 11:
```
a=13, b=11, result=0
11 is odd → result = 0+13=13, b=5, a=26
5 is odd → result = 13+26=39, b=2, a=52
2 is even → result=39, b=1, a=104
1 is odd → result = 39+104=143, b=0, a=208
Return 143
```

This implementation uses the binary representation concept where we only add multiples of a when the corresponding bit in b is 1.

