# Insertion Sort in Forth

Here's an implementation of the insertion sort algorithm in Forth:

```forth
\ Insertion Sort Implementation in Forth

\ Function to swap two elements in an array
: swap ( addr1 addr2 -- )
    2dup @ >r     \ Save value at addr1
    2dup @ rot !  \ Put value at addr2 into addr1
    r> !          \ Put saved value into addr2
;

\ Insertion sort algorithm
: insertion-sort ( array len -- )
    1 ( i = 1 )  \ Start with second element
    begin
        dup 1+ <               \ While i < len
    while
        \ Store current element in a temporary variable
        over +                 \ Get address of current element
        @                      \ Get value of current element
        tuck                   \ Put value back on stack, get address again
        >r                     \ Save address on return stack
        
        \ Inner loop: move elements greater than current element
        1- ( j = i - 1 )
        begin
            dup 0 >=           \ While j >= 0
        while
            over +             \ Get address of element at position j
            @                  \ Get value at position j
            r@ +               \ Get address of current element (r>)
            @                  \ Get current element value
            >                  \ Compare: if element[j] > current
        while
            \ Move element[j] to position [j+1]
            over +             \ Address of element[j]
            @                  \ Value at element[j]
            1+                 \ Address of element[j+1]
            !                  \ Store value at element[j+1]
            
            1-                 \ j--
        repeat
        
        \ Place current element in correct position
        r> +                   \ Get address of current element
        @                      \ Get current element value
        1+                     \ Address of new position
        !                      \ Store current element at new position
        
        1+                     \ i++
    repeat
    drop                       \ Clean up stack
;

\ Alternative simpler version for demonstration
: insert-sort ( array len -- )
    1 begin
        dup < while
        \ For each element, shift elements greater than it to the right
        over + @               \ Get current element value
        over +                 \ Get address of current element
        >r                     \ Save address on return stack
        
        0 swap                 \ j = 0
        begin
            r@ + @             \ Get element[j]
            r> 1+ + @          \ Get current element
            >                  \ Compare elements
        while
            r> 1+ + @          \ Get element[j] value
            r> + !             \ Store in element[j+1]
            r>                 \ Restore address
            1-                 \ j--
        repeat
        
        r> 1+ + !              \ Store current element at correct position
        1+                     \ i++
    repeat
    drop
;

\ Example usage:
\ Create an array: 5 2 8 1 9
\ : test-array 5 cells allot ;
\ 
\ 5 0 test-array !
\ 2 1 test-array !
\ 8 2 test-array !
\ 1 3 test-array !
\ 9 4 test-array !
\ 
\ test-array 5 insertion-sort
```

## How it works:

1. **Outer loop**: Iterate through each element starting from the second one (index 1)
2. **Inner loop**: Compare the current element with elements to its left
3. **Shift**: Move larger elements one position to the right
4. **Insert**: Place the current element in its correct sorted position

## Key Forth concepts used:

- **Stack manipulation**: Using `dup`, `swap`, `over`, `rot` for managing data
- **Address arithmetic**: Using `+` with cell addresses
- **Control structures**: `begin...while...repeat` loops
- **Return stack**: Using `r>` and `>r` to save/restore addresses

The algorithm has O(n²) time complexity in worst case, which is typical for insertion sort.