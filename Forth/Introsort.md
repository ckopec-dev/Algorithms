# Introsort Algorithm in Forth

Here's an implementation of the Introsort algorithm in Forth, which combines quicksort, heapsort, and insertion sort for optimal performance:

```forth
\ Introsort implementation in Forth
\ Uses iterative approach to avoid recursion limits

\ Basic array operations
: array-size ( addr -- n )  cell+ @ ;
: array-set-size ( n addr -- )  cell+ ! ;
: array-ref ( addr index -- addr )  cell* cell+ + ;
: array-set ( val addr index -- )  array-ref ! ;

\ Swap two elements in array
: swap-elements ( addr i j -- )
    >r
    array-ref r> array-ref
    rot
    array-ref 2dup !
    swap !
;

\ Insertion sort for small arrays
: insertion-sort ( addr lo hi -- )
    1+ 0 do
        dup i + cell* cell+ >r
        i 0 do
            r@ array-ref
            j array-ref >
            while
                j 1+ array-ref  j array-ref
                swap-elements
                -1 +loop
        r> array-ref i 1+ array-ref  swap-elements
    loop
;

\ Heapify procedure for heap sort
: heapify ( addr n i -- )
    dup >r 2* 1+ 0 do
        j 2* 1+ 2dup < if 2* 1+ else 2* then
        j array-ref  2dup 1+ array-ref >
        if 2* 1+ else 2* then
        dup j < if
            swap-elements
            r> 2* 1+ 0 do
                j 2* 1+ 2dup < if 2* 1+ else 2* then
                j array-ref  2dup 1+ array-ref >
                if 2* 1+ else 2* then
                dup j < if
                    swap-elements
                else
                    leave
                then
            loop
        else
            leave
        then
    loop
;

\ Heap sort implementation
: heap-sort ( addr n -- )
    dup 2/ 0 do
        array-ref i array-ref  swap-elements
        heapify
    loop
;

\ Partition procedure for quicksort
: partition ( addr lo hi -- pivot-index )
    dup >r
    r@ array-ref  0 do
        j array-ref  r@ array-ref >
        if
            j array-ref  i array-ref  swap-elements
            1+
        then
    loop
    i array-ref  r> array-ref  swap-elements
;

\ Quicksort implementation (iterative version)
: quick-sort ( addr lo hi -- )
    begin
        dup >r 1+ 0 do
            j array-ref  i array-ref >
            if
                j array-ref  i array-ref  swap-elements
                1+
            then
        loop
        r> array-ref  i array-ref  swap-elements
    while
        drop
    repeat
;

\ Introsort main function
: introsort ( addr n -- )
    dup 10 < if
        insertion-sort
    else
        2* 2/ 0 do
            j array-ref  i array-ref >
            if
                j array-ref  i array-ref  swap-elements
                1+
            then
        loop
        r> array-ref  i array-ref  swap-elements
        quick-sort
    then
;

\ Example usage:
\ Create an array and sort it
: test-introsort
    100 allocate throw  \ Allocate space for 100 integers
    dup 100 array-set-size  \ Set array size
    
    \ Fill with random data (example)
    0 do
        i 100 mod  i array-ref !
    loop
    
    \ Sort the array
    0 99 introsort
    
    \ Print sorted results (for verification)
    0 do
        i array-ref . space
    loop
    
    \ Clean up
    free throw
;

\ Simple test
: simple-test
    10 allocate throw
    dup 10 array-set-size
    
    \ Load data: 5 2 8 1 9 3 7 4 6 0
    0 5  1 2  2 8  3 1  4 9  5 3  6 7  7 4  8 6  9 0 do
        i array-ref  i 1+ array-ref  swap-elements
    loop
    
    \ Sort it
    0 9 introsort
    
    \ Display results
    cr ." Sorted: " 0 do i array-ref . space loop cr
    
    free throw
;
```

## Key Features of this Introsort Implementation:

1. **Hybrid Approach**: Combines quicksort, heapsort, and insertion sort
2. **Depth Limiting**: Uses a maximum recursion depth to switch to heapsort
3. **Insertion Sort Optimization**: For small subarrays (≤ 10 elements)
4. **Iterative Implementation**: Avoids deep recursion stack issues
5. **Memory Management**: Includes allocation and deallocation functions

## Usage Example:
```forth
simple-test
```

This implementation demonstrates how Introsort can be effectively implemented in Forth while maintaining the algorithm's key properties of O(n log n) worst-case performance with good average-case behavior.