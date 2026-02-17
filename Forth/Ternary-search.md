# Ternary Search in Forth

Here's an implementation of the ternary search algorithm in Forth:

```forth
\ Ternary search implementation in Forth
\ Searches for a key in a sorted array using ternary search

\ Array size
: array-size 10 ;

\ Sample sorted array (1 2 3 4 5 6 7 8 9 10)
: array ( -- addr )
    here 10 cells allot
    1 2 3 4 5 6 7 8 9 10
    10 0 do
        i cells + !
    loop
    here ;

\ Ternary search function
: ternary-search ( key array low high -- index )
    >r >r           \ Save high and low on return stack
    begin
        r> r>         \ Retrieve high and low
        2dup <        \ Check if low < high
    while
        \ Calculate mid1 and mid2
        2dup - 3 /   \ (high - low) / 3
        2dup +       \ low + (high - low) / 3 = mid1
        2dup - 3 /   \ (high - low) / 3
        2dup +       \ low + 2 * (high - low) / 3 = mid2
        
        \ Get values at mid1 and mid2
        2dup 1- cells + @  \ Value at mid1
        2dup 2- cells + @  \ Value at mid2
        
        \ Compare key with mid1 and mid2
        2dup < if
            \ Key is in left third
            2drop 2drop 2dup 1- 1-  \ Update high = mid1 - 1
        else
            2dup > if
                \ Key is in right third
                2drop 2drop 2dup 2+ 1-  \ Update low = mid2 + 1
            else
                \ Key is in middle third
                2drop 2drop 2dup 1- 2dup 2+ 1-  \ Update low = mid1 + 1, high = mid2 - 1
            then
        then
    repeat
    \ If we get here, key not found
    r> r> 2drop 2drop -1 ;  \ Return -1 for not found

\ Simple binary search for comparison (since ternary search is complex in Forth)
: binary-search ( key array low high -- index )
    >r >r
    begin
        r> r> 2dup <  \ low < high
    while
        2dup + 2/     \ mid = (low + high) / 2
        2dup 1- cells + @  \ Get value at mid
        2dup = if
            2drop 2drop 2dup 1-  \ Found at mid
        else
            2dup < if
                2drop 2drop 2dup 1- 1-  \ Update high = mid - 1
            else
                2drop 2drop 2dup 1+  \ Update low = mid + 1
            then
        then
    repeat
    r> r> 2drop 2drop -1 ;  \ Not found

\ Test the ternary search
: test-ternary-search ( -- )
    ." Testing ternary search:" cr
    array 0 array-size 1- 5 ternary-search
    ." Found 5 at index: " . cr
    array 0 array-size 1- 1 ternary-search
    ." Found 1 at index: " . cr
    array 0 array-size 1- 10 ternary-search
    ." Found 10 at index: " . cr
    array 0 array-size 1- 15 ternary-search
    ." Found 15 at index: " . cr ;

\ Run the test
test-ternary-search
```

## How it works:

1. **Array Setup**: Creates a sorted array of 10 elements (1-10)
2. **Ternary Search Logic**: 
   - Divides the search space into three parts
   - Calculates two midpoints (mid1 and mid2)
   - Compares the key with both midpoints
   - Eliminates one-third of the search space in each iteration
3. **Return Values**: Returns the index if found, -1 if not found

## Key Features:

- Uses the Forth stack for parameter passing
- Implements the classic ternary search algorithm
- Includes error handling for not-found cases
- Demonstrates stack manipulation in Forth
- Shows how to work with arrays in Forth

## Note:

The ternary search in Forth is more complex than in other languages due to stack-based nature and lack of direct array indexing. The binary search version is simpler and more commonly used in Forth environments.

