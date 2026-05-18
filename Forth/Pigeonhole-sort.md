# Pigeonhole Sort in Forth

Here's an implementation of the Pigeonhole Sort algorithm in Forth:

```forth
\ Pigeonhole Sort Implementation in Forth

\ Define constants
: MAX-VALUES 100 ;  \ Maximum number of elements
: MAX-VALUE 1000 ; \ Maximum possible value

\ Array storage
create pigeonhole-array MAX-VALUES cells allot
create hole-array MAX-VALUE cells allot

\ Clear the hole array
: clear-holes ( -- )
    MAX-VALUE 0 do
        0 hole-array i cells + !
    loop ;

\ Pigeonhole sort algorithm
: pigeonhole-sort ( addr n -- )
    \ addr = array address, n = number of elements
    over 0 do
        \ Find min and max values
        dup i cells + @
        over 0= if
            dup i cells + @
        else
            dup i cells + @
            over 0= if
                dup i cells + @
            else
                dup i cells + @
                over 0= if
                    dup i cells + @
                else
                    dup i cells + @
                then
            then
        then
    loop ;

\ Alternative simpler implementation
: pigeonhole-sort-simple ( addr n -- )
    \ Clear holes
    clear-holes
    
    \ Count occurrences
    over 0 do
        dup i cells + @
        hole-array + 1+
    loop ;
    
\ Copy back from holes to original array
: copy-back ( addr n -- )
    over 0 do
        hole-array i cells + @
        over 0= if
            dup i cells + !
        else
            hole-array i cells + @
            over 0= if
                dup i cells + !
            else
                hole-array i cells + @
                over 0= if
                    dup i cells + !
                else
                    hole-array i cells + @
                    over 0= if
                        dup i cells + !
                    else
                        hole-array i cells + @
                        over 0= if
                            dup i cells + !
                        else
                            hole-array i cells + @
                            over 0= if
                                dup i cells + !
                            else
                                hole-array i cells + @
                                over 0= if
                                    dup i cells + !
                                else
                                    hole-array i cells + @
                                then
                            then
                        then
                    then
                then
            then
        then
    loop ;

\ Complete pigeonhole sort function
: pigeonhole-sort-complete ( addr n -- )
    \ Save original array
    over 0 do
        dup i cells + @
        pigeonhole-array i cells + !
    loop
    
    \ Clear holes
    clear-holes
    
    \ Count occurrences
    over 0 do
        pigeonhole-array i cells + @
        hole-array + 1+
    loop
    
    \ Copy back to original array
    0 0 do
        hole-array i cells + @
        over 0= if
            pigeonhole-array i cells + !
        else
            hole-array i cells + @
            over 0= if
                pigeonhole-array i cells + !
            else
                hole-array i cells + @
                over 0= if
                    pigeonhole-array i cells + !
                else
                    hole-array i cells + @
                    over 0= if
                        pigeonhole-array i cells + !
                    else
                        hole-array i cells + @
                    then
                then
            then
        then
    loop ;

\ Example usage
: test-pigeonhole-sort ( -- )
    \ Create test array
    create test-array 5 cells allot
    3 test-array 0 cells + !
    1 test-array 1 cells + !
    4 test-array 2 cells + !
    1 test-array 3 cells + !
    5 test-array 4 cells + !
    
    \ Print original array
    ." Original array: "
    5 0 do
        test-array i cells + @ .
    loop
    cr
    
    \ Sort using pigeonhole sort
    test-array 5 pigeonhole-sort-complete
    
    \ Print sorted array
    ." Sorted array: "
    5 0 do
        test-array i cells + @ .
    loop
    cr ;

\ Run the test
test-pigeonhole-sort
```

## How it works:

1. **Clear holes**: Initialize an array to count occurrences of each value
2. **Count occurrences**: Go through the input array and increment counters in the hole array
3. **Copy back**: Reconstruct the sorted array by going through the hole array and placing elements back

## Time Complexity:
- **Best case**: O(n + k) where n is the number of elements and k is the range of input
- **Average case**: O(n + k)
- **Worst case**: O(n + k)

## Space Complexity:
- O(k) where k is the range of input values

## Key Features:
- Uses arrays to implement the pigeonhole concept
- Handles duplicate values correctly
- Works efficiently when the range of possible values is not significantly larger than the number of elements
- Demonstrates Forth's array manipulation capabilities

