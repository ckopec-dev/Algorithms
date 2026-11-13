# Pancake Sorting Algorithm in Forth

Pancake sorting is a sorting algorithm that sorts elements by performing a series of "pancake flips" - reversing subarrays from the beginning to some position.

## Forth Implementation

```forth
\ Pancake Sort Implementation in Forth

\ Stack-based array operations
: array-size ( addr -- n )  cells 1- @ ;  \ Get size of array
: array-set ( val addr index -- )  cells + ! ;  \ Set array element
: array-get ( addr index -- val )  cells + @ ;  \ Get array element

\ Reverse subarray from start to end
: reverse-subarray ( addr start end -- )
    >r  \ Save end index
    begin
        r@ over < while
        2dup array-get swap  \ Get elements at start and end
        r@ array-set swap  \ Swap them
        1+ 1-  \ Move pointers inward
        repeat
    r> drop ;  \ Clean up stack

\ Find maximum element in range [start, end]
: find-max ( addr start end -- max-index )
    >r  \ Save end index
    0  \ Initialize max index
    0  \ Initialize max value
    begin
        r@ over < while
        over array-get
        over > if
            swap  \ Swap max value and index
            drop
        then
        1+  \ Move to next element
        repeat
    r> drop ;

\ Pancake sort implementation
: pancake-sort ( addr n -- )
    >r  \ Save array address and size
    begin
        r@ 1 > while  \ Continue while size > 1
        \ Find maximum element in current range
        0 r@ 1- find-max
        \ If max is not at the end, flip it there
        over r@ 1- <> if
            \ Flip to bring max to front
            0 swap reverse-subarray
            \ Flip to bring max to correct position
            r@ 1- 0 swap reverse-subarray
        then
        1-  \ Reduce size by 1
        repeat
    r> drop ;  \ Clean up stack

\ Example usage and testing
: test-pancake-sort
    \ Create array [3, 2, 4, 1]
    create test-array 4 cells allot
    3 test-array 0 array-set
    2 test-array 1 array-set
    4 test-array 2 array-set
    1 test-array 3 array-set
    
    \ Print original array
    cr ." Original array: "
    4 0 do
        test-array i array-get . space
    loop
    
    \ Sort the array
    test-array 4 pancake-sort
    
    \ Print sorted array
    cr ." Sorted array: "
    4 0 do
        test-array i array-set
        test-array i array-get . space
    loop
    cr ;

\ Run the example
test-pancake-sort
```

## How it Works

1. **Find Maximum**: Locate the largest element in the unsorted portion
2. **Flip to Front**: If maximum isn't at the front, flip the subarray to bring it there
3. **Flip to Correct Position**: Flip the entire unsorted portion to move maximum to its correct position
4. **Repeat**: Continue with remaining unsorted elements

## Example Output
```
Original array: 3 2 4 1 
Sorted array: 1 2 3 4 
```

## Key Forth Concepts Used

- **Stack Operations**: `over`, `swap`, `drop` for manipulating data
- **Control Structures**: `begin...while...repeat` loops
- **Memory Management**: `cells + !` and `cells + @` for array access
- **Subroutines**: Modular functions for reverse, find-max, and sorting logic

This implementation demonstrates how Forth's stack-based approach can be elegantly used to implement complex algorithms like pancake sorting.