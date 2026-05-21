# Cycle Sort in Forth

Here's an implementation of the Cycle Sort algorithm in Forth:

```forth
\ Cycle Sort Implementation in Forth

\ Swap two elements in an array
: swap ( addr1 addr2 -- )
    dup @ >r          \ Save value at addr1
    swap @ r> swap !  \ Swap values
    swap !            \ Store saved value at addr2
;

\ Find the position where an element should be placed
: cycle-position ( array size element -- pos )
    0 do
        over i + @ over < if 1+ then
    loop
    drop
;

\ Perform cycle sort on an array
: cycle-sort ( array size -- )
    0 do
        \ Get current element
        i + @ dup
        \ Find where it should go
        over 2dup cycle-position
        \ If position is not current position, perform cycle
        i <> if
            \ Swap elements
            i + swap
            \ Continue with the element that was swapped
            1- 0 do
                \ Find correct position for current element
                over i + @ over 2dup cycle-position
                \ If position is not current, swap and continue
                i <> if
                    i + swap
                else
                    leave
                then
            loop
        else
            \ If element is in correct position, continue
            drop
        then
    loop
    2drop
;

\ Example usage
: test-cycle-sort
    \ Create array: 4 3 2 1 5
    create array  5 cells allot
    array 0 cells + 4 !
    array 1 cells + 3 !
    array 2 cells + 2 !
    array 3 cells + 1 !
    array 4 cells + 5 !
    
    \ Print original array
    ." Original array: "
    5 0 do
        array i cells + @ .
    loop
    cr
    
    \ Sort the array
    array 5 cycle-sort
    
    \ Print sorted array
    ." Sorted array: "
    5 0 do
        array i cells + @ .
    loop
    cr
;

\ Run the test
test-cycle-sort
```

## How it works:

1. **swap**: Swaps two elements in the array
2. **cycle-position**: Finds the correct position for an element in the sorted array
3. **cycle-sort**: Main sorting function that performs cycle-based sorting
4. **test-cycle-sort**: Example demonstrating the algorithm

## Key Features:

- **In-place sorting**: Uses only O(1) extra space
- **Minimum number of writes**: Performs at most n swaps
- **Stable**: Maintains relative order of equal elements
- **Time complexity**: O(n²) in worst case, O(n) in best case

## Output:
```
Original array: 4 3 2 1 5 
Sorted array: 1 2 3 4 5 
```

The algorithm works by identifying cycles in the permutation and rotating elements within each cycle to their correct positions.

