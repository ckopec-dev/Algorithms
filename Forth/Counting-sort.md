# Counting Sort in Forth

Here's an implementation of the counting sort algorithm in Forth:

```forth
\ Counting Sort Implementation in Forth

\ Define constants
: MAX-VALUE 100 ;  \ Maximum possible value in array

\ Counting Sort Algorithm
: counting-sort ( addr len -- )
    dup 0= if exit then  \ If length is 0, exit
    
    \ Create counting array
    MAX-VALUE 1+ cells allocate throw
    dup 0 over + 0 do 0 swap ! loop  \ Initialize all elements to 0
    
    \ Count occurrences of each element
    over + 0 do
        i @ 1+  \ Increment count for this value
        dup 1- cells + 1+  \ Move to correct position and increment
    loop
    
    \ Reconstruct sorted array
    over + 0 do
        i @ 0 do
            i 1- cells + @  \ Get count for this value
            0 ?do
                i 1- cells + 1-  \ Decrement count
                i 1- cells + @ 1-  \ Get value
                i 1- cells + 1-  \ Move to correct position
                i 1- cells + 1-  \ Increment count
            loop
        loop
    loop
    
    \ Free allocated memory
    free throw
;

\ Alternative simpler version for small range
: counting-sort-simple ( addr len -- )
    dup 0= if exit then
    
    \ Allocate counting array (assuming values 0-99)
    100 cells allocate throw
    dup 0 over + 0 do 0 swap ! loop  \ Initialize to 0
    
    \ Count elements
    over + 0 do
        i @ cells + 1+  \ Increment count for value
    loop
    
    \ Reconstruct sorted array
    over 0 do
        0 100 do
            i cells + @ 0 ?do
                i 1- cells + 1-  \ Decrement count
                i 1- cells + 1-  \ Store value in output
            loop
        loop
    loop
    
    \ Free memory
    free throw
;

\ Example usage:
\ Create test array
: test-array ( -- )
    5 cells allocate throw
    4 0 !  \ Array elements: 4 2 2 8 3
    2 1 !
    2 2 !
    8 3 !
    3 4 !
    5 0  \ Length = 5
    counting-sort  \ Sort the array
    \ Print results
    0 do
        i cells + @ . loop
    5 0 do
        i cells + 0 !  \ Clear array
    loop
    free throw
;

\ More practical version with proper sorting
: counting-sort-fixed ( addr len -- )
    dup 0= if exit then
    
    \ Allocate counting array
    100 cells allocate throw
    dup 0 over + 0 do 0 swap ! loop  \ Initialize to 0
    
    \ Count occurrences
    over + 0 do
        i @ cells + 1+  \ Increment count
    loop
    
    \ Reconstruct sorted array
    0 100 do
        i cells + @ 0 ?do
            i 1- cells + 1-  \ Decrement count
            i 1- cells + 1-  \ Store value
        loop
    loop
    
    \ Free memory
    free throw
;
```

## Key Features:

1. **Memory Management**: Uses `allocate` and `free` for dynamic memory allocation
2. **Counting Array**: Creates a counting array to track occurrences
3. **Sorting Logic**: Builds the sorted array by reconstructing from counts
4. **Error Handling**: Uses `throw` for proper error handling

## Usage:
```forth
\ Example:
\ Create array with 5 elements
5 cells allocate throw
4 0 ! 2 1 ! 2 2 ! 8 3 ! 3 4 !
5 0  counting-sort  \ Sort array of 5 elements
```

This implementation demonstrates the counting sort algorithm's linear time complexity O(n+k) where n is the number of elements and k is the range of input.

