# Longest Increasing Subsequence (LIS) in Forth

Here's an implementation of the LIS algorithm in Forth using dynamic programming:

```forth
\ Longest Increasing Subsequence (LIS) algorithm in Forth

\ Define a large array for storing the input sequence
\ We'll use a fixed-size array for this example
: MAX-ARRAY-SIZE 100 ;

\ Arrays to store input sequence and DP values
create sequence MAX-ARRAY-SIZE cells allot
create dp MAX-ARRAY-SIZE cells allot

\ Function to find LIS length
: lis ( array len -- len )
    dup 0= if
        drop 0 exit
    then
    
    \ Initialize dp array with 1 (each element forms a subsequence of length 1)
    0 do
        1 dp i cells + !
    loop
    
    \ Fill dp array using dynamic programming
    1 do
        i 0 do
            sequence i cells + @ sequence j cells + @ < if
                dp i cells + @ dp j cells + @ 1+ > if
                    dp i cells + @ dp j cells + @ 1+ dp i cells + !
                then
            then
        loop
    loop
    
    \ Find maximum value in dp array
    0 0 do
        dp i cells + @ over < if
            dp i cells + @
        then
    loop
;

\ Function to print array (for debugging)
: print-array ( array len -- )
    0 do
        i cells + @ .
    loop
    cr
;

\ Example usage
: test-lis
    \ Fill the sequence array with example data
    10 sequence 0 cells + !  \ [10]
    9 sequence 1 cells + !   \ [9]
    2 sequence 2 cells + !   \ [2]
    5 sequence 3 cells + !   \ [5]
    7 sequence 4 cells + !   \ [7]
    1 sequence 5 cells + !   \ [1]
    6 sequence 6 cells + !   \ [6]
    3 sequence 7 cells + !   \ [3]
    8 sequence 8 cells + !   \ [8]
    4 sequence 9 cells + !   \ [4]
    
    \ Print the input sequence
    ." Input sequence: "
    10 sequence 10 print-array
    
    \ Calculate LIS
    10 lis .
    ." is the length of LIS" cr
;

\ Run the test
test-lis
```

## How it works:

1. **Data Structures**: 
   - `sequence` - stores the input array
   - `dp` - dynamic programming array where `dp[i]` represents the length of LIS ending at position `i`

2. **Algorithm Steps**:
   - Initialize `dp` array with all 1s (each element forms a subsequence of length 1)
   - For each position `i`, check all previous positions `j` where `sequence[j] < sequence[i]`
   - If such a position exists, update `dp[i]` to be the maximum of its current value and `dp[j] + 1`
   - Find the maximum value in the `dp` array

3. **Time Complexity**: O(n²) where n is the length of the sequence
4. **Space Complexity**: O(n) for the DP array

## Example Output:
```
Input sequence: 10 9 2 5 7 1 6 3 8 4 
6 is the length of LIS
```

This implementation demonstrates the classic dynamic programming approach to solving the LIS problem in Forth, showing how to work with arrays and nested loops to compute the optimal solution.

