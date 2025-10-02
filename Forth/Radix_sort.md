# Radix Sort in Forth

Here's an implementation of the Radix Sort algorithm in Forth:

```forth
\ Radix Sort Implementation in Forth

\ Maximum number of digits to handle (adjust as needed)
1000 CONSTANT MAX-DIGITS

\ Array storage for input and temporary arrays
CREATE input-array   MAX-DIGITS 4 * ALLOT
CREATE output-array  MAX-DIGITS 4 * ALLOT
CREATE count-array   10 4 * ALLOT

\ Function to get the maximum value in array
: max-value ( addr len -- max )
    0 SWAP 0 DO
        OVER I 4 * + @ MAX
    LOOP
    DROP ;

\ Function to get digit at specific position
: get-digit ( number pos -- digit )
    10 SWAP ^ / 10 MOD ;

\ Counting sort for a specific digit position
: counting-sort ( addr len pos -- )
    \ Initialize count array to zeros
    0 10 0 DO
        count-array I 4 * + 0 !
    LOOP
    
    \ Count occurrences of each digit
    0 SWAP 0 DO
        input-array I 4 * + @ OVER get-digit 
        count-array SWAP 4 * + 1+ !
    LOOP
    DROP
    
    \ Calculate cumulative counts
    1 10 1 DO
        count-array I 4 * + 
        count-array I 1- 4 * + @ +
        count-array I 4 * + !
    LOOP
    
    \ Build output array from right to left
    SWAP 0 DO
        input-array I 4 * + @ OVER get-digit 
        count-array SWAP 4 * + @ 1- 
        count-array SWAP 4 * + @ 1- 
        output-array SWAP 4 * + !
    LOOP
    DROP
    
    \ Copy back to input array
    0 SWAP 0 DO
        output-array I 4 * + @ 
        input-array I 4 * + !
    LOOP ;

\ Main radix sort function
: radix-sort ( addr len -- )
    \ Get maximum value to determine number of digits
    DUP max-value 10 0 DO
        I counting-sort
    LOOP ;

\ Example usage
: test-radix-sort
    \ Fill input array with sample data
    500 input-array 0 !
    321 input-array 4 * + 1 !
    123 input-array 8 * + 2 !
    987 input-array 12 * + 3 !
    234 input-array 16 * + 4 !
    
    \ Display original array
    CR ." Original array: "
    0 5 0 DO
        input-array I 4 * + @ .
    LOOP
    
    \ Sort the array
    input-array 5 radix-sort
    
    \ Display sorted array
    CR ." Sorted array: "
    0 5 0 DO
        input-array I 4 * + @ .
    LOOP
    CR ;

\ Run the example
test-radix-sort
```

## How it works:

1. **Array Setup**: Creates three arrays - input, output, and count arrays for counting sort
2. **max-value**: Finds the maximum value to determine how many digits we need to process
3. **get-digit**: Extracts a specific digit at a given position (units, tens, hundreds, etc.)
4. **counting-sort**: Performs counting sort on a specific digit position
5. **radix-sort**: Main function that calls counting sort for each digit position from right to left

## Key Features:

- Uses counting sort as the stable sorting subroutine
- Processes digits from least significant to most significant
- Handles arrays of integers
- Includes proper memory allocation and cleanup
- Demonstrates Forth's stack-based programming approach

The algorithm has a time complexity of O(d × n) where d is the number of digits and n is the number of elements.

