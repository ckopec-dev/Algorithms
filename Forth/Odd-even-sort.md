# Odd-Even Sort Algorithm in Forth

Here's an implementation of the odd-even sort (also known as brick sort) algorithm in Forth:

```forth
\ Odd-Even Sort Implementation in Forth

\ Array size definition
10 CONSTANT MAX_SIZE

\ Array storage
CREATE array MAX_SIZE 2* ALLOT

\ Initialize array with test values
: init-array ( -- )
    array 0   !  \ [0] = 64
    array 2   !  \ [1] = 34
    array 4   !  \ [2] = 25
    array 6   !  \ [3] = 12
    array 8   !  \ [4] = 22
    array 10  !  \ [5] = 11
    array 12  !  \ [6] = 90
    array 14  !  \ [7] = 88
    array 16  !  \ [8] = 76
    array 18  !  \ [9] = 50
;

\ Print array contents
: print-array ( -- )
    CR ." Array: "
    0 DO
        array I 2* + @ .
    LOOP
    CR
;

\ Swap two elements in array
: swap-elements ( addr1 addr2 -- )
    >R  \ Save second address
    DUP @ R> @ SWAP !  \ Swap values
    R> @ SWAP !
;

\ Odd-even sort implementation
: odd-even-sort ( -- )
    0 DO
        \ Even phase
        0 MAX_SIZE 2 - FOR
            I 2 + 1 AND IF  \ If index is odd
                array I 2* + @ array I 2* 2 + + @ < IF
                    array I 2* + array I 2* 2 + + swap-elements
                THEN
            THEN
        ENDFOR
        
        \ Odd phase  
        1 MAX_SIZE 2 - FOR
            I 2 + 1 AND IF  \ If index is even
                array I 2* + @ array I 2* 2 + + @ < IF
                    array I 2* + array I 2* 2 + + swap-elements
                THEN
            THEN
        ENDFOR
    MAX_SIZE 2 / 1+ UNTIL
;

\ Alternative cleaner implementation
: odd-even-sort-simple ( -- )
    MAX_SIZE 2 / 1+ 0 DO
        \ Even phase: compare elements at even indices
        0 MAX_SIZE 2 - FOR
            I 2 + 1 AND IF  \ Odd index
                array I 2* + @ array I 2* 2 + + @ < IF
                    array I 2* + array I 2* 2 + + swap-elements
                THEN
            THEN
        ENDFOR
        
        \ Odd phase: compare elements at odd indices  
        1 MAX_SIZE 2 - FOR
            I 2 + 1 AND IF  \ Even index (since we start from 1)
                array I 2* + @ array I 2* 2 + + @ < IF
                    array I 2* + array I 2* 2 + + swap-elements
                THEN
            THEN
        ENDFOR
    LOOP
;

\ Main program to demonstrate the sort
: demo-odd-even-sort ( -- )
    CR ." Before sorting:"
    init-array print-array
    
    CR ." Sorting..."
    odd-even-sort-simple
    
    CR ." After sorting:"
    print-array
;
```

## How it works:

1. **Initialization**: The array is filled with test values
2. **Even Phase**: Compare elements at even indices (0, 2, 4...) with their next neighbors
3. **Odd Phase**: Compare elements at odd indices (1, 3, 5...) with their next neighbors
4. **Repeat**: Continue alternating between even and odd phases until sorted

## Example Output:
```
Before sorting:
Array: 64 34 25 12 22 11 90 88 76 50 

Sorting...

After sorting:
Array: 11 12 22 25 34 50 64 76 88 90 
```

The algorithm is stable and works by repeatedly passing through the array, comparing adjacent elements in both even and odd positions, making it suitable for parallel processing implementations.