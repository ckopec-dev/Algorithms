# Randomized Quicksort in Forth

Here's an implementation of the Randomized Quicksort algorithm in Forth:

```forth
\ Randomized Quicksort implementation in Forth

\ Stack-based random number generator (simple linear congruential generator)
0 value rand-seed

: random-init ( -- )  12345 rand-seed ! ;  \ Initialize with seed

: random ( -- n )  \ Generate random number between 0 and 32767
    rand-seed @ 1103515245 + 12345 * 2147483647 and
    rand-seed ! ;

: random-range ( max -- n )  \ Generate random number 0..max-1
    random swap mod ;

\ Swap two elements in array
: swap-elements ( array i j -- )
    over +  over +  2dup 2@  rot 2@  rot 2!
    rot 2! ;

\ Partition function with random pivot selection
: partition ( array low high -- pivot-index )
    2dup - 1+  random-range  +  \ Get random index in range
    over +  2dup 2@  rot 2!  \ Move random element to end
    rot 2!  \ Put pivot at end
    
    2dup 1-  over +  2@  \ Get pivot value
    2dup 1-  0  \ Initialize i = low-1
    do
        over +  2@  over 2@  \ Compare array[j] with pivot
        <  if
            1+  over +  2dup 2@  rot 2!  \ Swap elements
            rot 2!  \ Put pivot back
        then
    loop
    1+  over +  2dup 2@  rot 2!  \ Put pivot in correct position
    rot 2!  \ Put pivot back
    ;

\ Recursive quicksort
: quicksort ( array low high -- )
    2dup <  if
        partition  \ Get pivot index
        2dup 1-  quicksort  \ Sort left subarray
        1+  quicksort  \ Sort right subarray
    then
    ;

\ Main quicksort function
: randomized-quicksort ( array size -- )
    0 swap 1-  quicksort ;

\ Example usage
create test-array  50 ,  23 ,  87 ,  12 ,  45 ,  67 ,  34 ,  98 ,  11 ,  76 ,

: test-quicksort ( -- )
    cr ." Original array: "
    10 0 do
        test-array i +  @  .  space
    loop
    cr ." Sorting..."
    test-array 10 randomized-quicksort
    cr ." Sorted array: "
    10 0 do
        test-array i +  @  .  space
    loop
    cr ;

\ Initialize random generator and run test
random-init
test-quicksort
```

## Key Features of this Implementation:

1. **Random Pivot Selection**: Uses `random-range` to select a random pivot index
2. **In-place Sorting**: Sorts the array without requiring additional memory
3. **Recursive Structure**: Uses the standard recursive quicksort approach
4. **Stack-based Operations**: Leverages Forth's stack-based nature for efficient operations
5. **Simple Random Generator**: Uses a basic LCG (Linear Congruential Generator) for random number generation

## How it Works:

1. **Random Initialization**: Sets up a simple random number generator
2. **Partitioning**: Randomly selects a pivot element and partitions the array
3. **Recursive Sorting**: Recursively sorts the subarrays on both sides of the pivot
4. **In-place Swapping**: Uses stack operations to efficiently swap array elements

The algorithm maintains the expected O(n log n) time complexity with high probability due to the random pivot selection, avoiding the worst-case O(n²) behavior of standard quicksort.

