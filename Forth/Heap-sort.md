# Heap Sort in Forth

Here's an implementation of the Heap Sort algorithm in Forth:

```forth
\ Heap Sort Implementation in Forth

\ Heap sort requires array operations and heap manipulation
\ We'll use a simple array-based approach

\ Define array size
100 CONSTANT MAX-ARRAY-SIZE

\ Array storage
CREATE heap-array MAX-ARRAY-SIZE ALLOT

\ Helper functions for heap operations

\ Parent index of node at index i
: parent ( i -- parent )
    1- 2/ ;

\ Left child index of node at index i
: left-child ( i -- left )
    1+ 2* ;

\ Right child index of node at index i
: right-child ( i -- right )
    2+ 2* ;

\ Swap two elements in array
: swap ( i j -- )
    >R
    heap-array + CELL+ @  \ Get element at i
    heap-array + CELL+ @  \ Get element at j
    R> heap-array + CELL+ !  \ Put element j at i
    heap-array + CELL+ !  \ Put element i at j
;

\ Heapify operation - maintain max-heap property
: heapify ( i heap-size -- )
    >R
    2DUP left-child R@ < IF
        left-child
        2DUP heap-array + CELL+ @  \ Get left child
        heap-array + CELL+ @  \ Get current node
        > IF
            swap
        THEN
    THEN
    2DUP right-child R@ < IF
        right-child
        2DUP heap-array + CELL+ @  \ Get right child
        heap-array + CELL+ @  \ Get current node
        > IF
            swap
        THEN
    THEN
    R> DROP
;

\ Build max heap from array
: build-heap ( heap-size -- )
    2DUP 2/ 1+ 0 DO
        I heapify
    LOOP
;

\ Heap sort main function
: heap-sort ( array-size -- )
    >R
    R@ build-heap
    R@ 1- 0 DO
        0 R@ swap
        R@ 1- heapify
    LOOP
    R> DROP
;

\ Example usage:
\ Fill array with sample data
: fill-sample-array ( -- )
    heap-array 0 !  50
    heap-array CELL+ 1 !  100
    heap-array 2 CELL+ 2 !  30
    heap-array 3 CELL+ 3 !  70
    heap-array 4 CELL+ 4 !  20
    heap-array 5 CELL+ 5 !  90
    heap-array 6 CELL+ 6 !  40
    heap-array 7 CELL+ 7 !  80
    heap-array 8 CELL+ 8 !  60
    heap-array 9 CELL+ 9 !  10
;

\ Print array elements
: print-array ( size -- )
    0 DO
        heap-array I CELL+ @ .
    LOOP
    CR
;

\ Main test routine
: test-heap-sort ( -- )
    10 fill-sample-array
    ." Before sorting: " 
    10 print-array
    10 heap-sort
    ." After sorting:  " 
    10 print-array
;

\ Run the test
test-heap-sort
```

## How it works:

1. **Array Setup**: Creates a heap array of maximum size 100
2. **Helper Functions**: 
   - `parent`, `left-child`, `right-child` - Calculate heap indices
   - `swap` - Exchange two array elements
   - `heapify` - Maintain max-heap property
3. **Build Heap**: Converts array into max-heap structure
4. **Sort**: Repeatedly extracts maximum element and rebuilds heap

## Key Features:

- Uses cell-based array addressing
- Implements standard heap sort algorithm with O(n log n) complexity
- Includes proper heap maintenance operations
- Demonstrates Forth's stack-based programming paradigm

The algorithm sorts the array in ascending order by building a max-heap and repeatedly extracting the maximum element.

