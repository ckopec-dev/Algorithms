# Exponential Search Algorithm in Forth

Here's an implementation of the exponential search algorithm in Forth:

```forth
\ Exponential Search Algorithm in Forth
\ Searches for a key in a sorted array using exponential search

\ Array size (modify as needed)
: ARRAY-SIZE 1000 ;

\ Sample sorted array (for demonstration)
\ In practice, this would be passed as parameter or defined elsewhere
:array my-array ARRAY-SIZE cells allot

\ Binary search helper function for exponential search
: binary-search ( key addr len -- index|-1 )
    >r  \ Save length on return stack
    0     \ low = 0
    r>    \ Get length back
    1-    \ high = length - 1
    begin
        2dup > not  \ while low <= high
    while
        2dup + 2/  \ mid = (low + high) / 2
        r@ cells my-array + @  \ get array[mid]
        2over 2over  \ key array[mid] key array[mid]
        >  if
            1+  \ low = mid + 1
        else
            2over 2over  \ key array[mid] key array[mid]
            <  if
                1-  \ high = mid - 1
            else
                2drop 2drop  \ Found it, return index
                exit
            then
        then
    repeat
    drop 2drop  \ No match found, return -1
    -1 ;

\ Exponential search function
: exponential-search ( key addr len -- index|-1 )
    1     \ i = 1
    begin
        2dup  \ key addr len key addr len
        r@ cells my-array + @  \ key array[i]
        2over  \ key array[i] key array[i]
        >  if  \ if key > array[i]
            2*  \ i = i * 2
        else
            2drop 2drop  \ Remove extra copies of key and addr
            r>  \ Get the original length
            1-  \ high = i - 1
            r@  \ Push high again for binary search
            0   \ low = 0
            2swap  \ Put high and low in correct order
            2swap  \ Put key and addr in correct order
            binary-search  \ Call binary search on range [0, high]
            exit
        then
    repeat
    \ If we get here, we need to check if key is at position i
    r>  \ Get the original length
    1-  \ high = i - 1
    2swap  \ Put key and addr in correct order
    0   \ low = 0
    2swap  \ Put high and low in correct order
    2swap  \ Put key and addr in correct order
    binary-search  \ Call binary search on range [0, high]
    drop 2drop ;

\ Example usage:
: test-exponential-search ( -- )
    \ Fill array with sample data (0 to 999)
    0 0 do
        i my-array i cells + !
    loop
    
    \ Search for key 42
    42 my-array ARRAY-SIZE exponential-search
    . ." Found at index: " .
    
    \ Search for key that doesn't exist
    1000 my-array ARRAY-SIZE exponential-search
    . ." Found at index: " . ;

\ Alternative simpler version with fixed array:
: simple-exp-search ( key -- index|-1 )
    1     \ i = 1
    begin
        2dup  \ key key
        r@ cells my-array + @  \ key array[i]
        >  if  \ if key > array[i]
            2*  \ i = i * 2
        else
            \ Found range, do binary search on [0, i-1]
            2drop  \ Remove extra copy of key
            r>  \ Get i back
            1-  \ high = i - 1
            0   \ low = 0
            2swap  \ Put key and addr in correct order
            binary-search  \ Call binary search
            exit
        then
    repeat ;

\ Test the implementation
: run-tests ( -- )
    ." Testing exponential search..." cr
    test-exponential-search ;
```

## How it works:

1. **Exponential Search** starts by checking if the key exists at index 1, then 2, then 4, 8, etc.
2. When we find an element greater than our key, we know the key must be in the range [prev_index+1, current_index]
3. We then perform a **binary search** on that range
4. The time complexity is O(log n) for the exponential phase and O(log log n) for the binary search phase

## Key Forth Concepts Used:

- **Stack manipulation**: `2dup`, `2swap`, `r>`, `r@` for managing data flow
- **Loops**: `begin...while...repeat` for iterative searching
- **Conditional execution**: `if...else...then` for decision making
- **Cell addressing**: `cells my-array + @` for array access
- **Return stack operations**: `r@`, `r>` for temporary data storage

This implementation demonstrates how to translate a complex algorithm into Forth's stack-based programming paradigm.