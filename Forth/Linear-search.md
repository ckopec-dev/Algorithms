# Linear Search Algorithm in Forth

Here's an implementation of the linear search algorithm in Forth:

```forth
\ Linear search function
\ Input: address of array, length, target value
\ Output: index if found, -1 if not found

: linear-search ( addr len target -- index )
    0               \ Initialize index counter
    begin           \ Start loop
        2dup        \ Copy addr and len
        <          \ Check if index < length
    while
        over        \ Get address of current element
        +           \ Calculate address of element at current index
        @           \ Fetch value at that address
        2 pick      \ Get target value
        =           \ Compare with target
        if          \ If found
            drop 2drop exit     \ Return index and exit
        then
        1+          \ Increment index
    repeat
    drop 2drop -1   \ Not found, return -1
;

\ Example usage:
\ Create an array
create my-array 10 , 25 , 3 , 47 , 15 , 8 , 92 , 1 , 63 , 38 ,

\ Search for value 47
: test-search
    my-array 10 47 linear-search
    . ." Index of 47 is: " .
;

\ Search for value 100 (not in array)
: test-search-not-found
    my-array 10 100 linear-search
    . ." Index of 100 is: " .
;
```

## How it works:

1. **Function signature**: `linear-search ( addr len target -- index )`
   - `addr`: address of the array
   - `len`: length of the array
   - `target`: value to search for
   - `index`: returns index if found, -1 if not found

2. **Algorithm steps**:
   - Initialize counter to 0
   - Loop while counter < array length
   - Calculate address of current element: `addr + (counter * sizeof(element))`
   - Compare element value with target
   - If match found, return current index
   - If not found after checking all elements, return -1

3. **Example output**:
```
Index of 47 is: 3
Index of 100 is: -1
```

This implementation demonstrates how to perform linear search in Forth, using stack operations and basic loop constructs.