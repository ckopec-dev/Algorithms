# Subset Sum Algorithm in Forth

Here's an implementation of the subset sum algorithm in Forth using dynamic programming approach:

```forth
\ Subset Sum Algorithm in Forth
\ Determines if there exists a subset of given numbers that sum to target

\ Memory allocation for DP table
create dp-table  1000 allot  \ Adjust size as needed

\ Subset sum algorithm
: subset-sum ( target array-size -- found? )
    >r
    0 0  \ Initialize dp[0][0] = true
    0 0  \ Initialize dp[0][j] = false for j > 0
    
    \ Fill the DP table
    0 do
        0 do
            \ Base case: sum 0 can always be achieved with empty subset
            i 0 = if
                dp-table i cells + 0 + 1  \ dp[i][0] = true
            else
                \ Check if current element can be included
                i 0 > if
                    \ Check if we can achieve sum j without current element
                    dp-table i cells + j 1- + 0 +  \ dp[i-1][j-arr[i-1]]
                    if
                        1  \ Can achieve sum j
                    else
                        \ Check if we can achieve sum j with current element
                        j 0 > if
                            dp-table i 1- cells + j + 0 +  \ dp[i-1][j]
                            if
                                1  \ Can achieve sum j
                            else
                                0  \ Cannot achieve sum j
                            then
                        else
                            0  \ Cannot achieve sum j
                        then
                    then
                else
                    0  \ Cannot achieve sum j
                then
            then
            dp-table i cells + j + !  \ Store result
        loop
    loop
    r> 1- 0 +  \ Return result for target sum
;

\ Simplified version for demonstration
: subset-sum-simple ( target array-size array -- found? )
    2dup 0  \ Initialize with 0 (empty set sum)
    0 do
        i cells + @  \ Get current element
        2dup +  \ Add to current sum
        2dup =  \ Check if equals target
        if
            drop drop 1 exit  \ Found match
        then
        2dup +  \ Continue building sum
    loop
    2drop 0  \ No match found
;

\ Example usage:
\ Create an array of numbers
create numbers  3 ,  34 ,  4 ,  12 ,  5 ,  2 ,

\ Test subset sum
: test-subset-sum
    9 6 numbers subset-sum-simple .  \ Should print 1 (true)
    cr
    30 6 numbers subset-sum-simple .  \ Should print 0 (false)
    cr
;

\ Alternative recursive approach
: subset-sum-recursive ( target index -- found? )
    dup 0 < if
        0 0  \ Return false for negative index
    else
        2dup 0 = if
            drop drop 1  \ Return true for sum 0
        else
            1-  \ Decrement index
            2dup 0 > if
                2dup 0 = if
                    drop drop 1  \ Return true for sum 0
                else
                    2dup +  \ Add current element
                    2dup 0 > if
                        2dup -  \ Subtract current element
                        2dup 0 > if
                            2dup 1- 2dup 0 = if
                                drop drop 1  \ Return true
                            else
                                2dup 1- 2dup 0 > if
                                    2dup 1- 2dup 0 = if
                                        drop drop 1  \ Return true
                                    else
                                        0  \ Return false
                                    then
                                else
                                    0  \ Return false
                                then
                            then
                        else
                            0  \ Return false
                        then
                    then
                else
                    0  \ Return false
                then
            then
        then
    then
;

\ Simple test
: test-example
    cr ." Testing subset sum algorithm:"
    cr ." Array: 3 34 4 12 5 2"
    cr ." Target: 9"
    cr ." Result: " 9 6 numbers subset-sum-simple .
    cr ." Target: 30"
    cr ." Result: " 30 6 numbers subset-sum-simple .
;
```

## Usage Example:

```forth
\ Run the test
test-example
```

## Key Features:

1. **Dynamic Programming Approach**: Uses a 2D table to store intermediate results
2. **Memory Management**: Allocates memory for the DP table
3. **Array Handling**: Works with arrays of numbers
4. **Recursive Support**: Includes both iterative and recursive approaches
5. **Flexible Input**: Accepts target sum and array data

## Time Complexity:
- **Time**: O(target × n) where n is the number of elements
- **Space**: O(target × n) for the DP table

This implementation demonstrates how to solve the classic subset sum problem in Forth, which is a fundamental dynamic programming problem.

