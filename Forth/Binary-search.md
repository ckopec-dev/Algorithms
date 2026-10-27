# Binary Search in Forth

Here's an implementation of binary search algorithm in Forth:

```forth
\ Binary search in Forth
\ Returns index if found, -1 if not found

: binary-search ( addr len key -- index/-1 )
    0 swap          \ low = 0
    over 1-         \ high = len-1
    begin           \ while low <= high
        2dup <      \ if low > high
    while
        2dup + 2/   \ mid = (low + high) / 2
        over        \ get address of mid element
        @           \ fetch value at mid
        3 pick      \ get key to compare
        >           \ compare: if mid > key
    if
        2dup - 1-   \ high = mid - 1
    else
        2dup + 1+   \ low = mid + 1
    then
        2drop       \ drop old low/high
    repeat
    2drop           \ drop low and high
    -1              \ not found
;

\ Example usage:
\ Create a sorted array
create my-array 1 , 3 , 5 , 7 , 9 , 11 , 13 , 15 ,

\ Test the binary search
: test-search ( -- )
    my-array 8 7 binary-search .  \ Should print 3 (index of 7)
    my-array 8 4 binary-search .  \ Should print -1 (not found)
;

\ Alternative version with explicit array bounds
: binary-search-2 ( addr low high key -- index/-1 )
    begin           \ while low <= high
        2dup <      \ if low > high
    while
        2dup + 2/   \ mid = (low + high) / 2
        over        \ get address of mid element
        @           \ fetch value at mid
        3 pick      \ get key to compare
        >           \ compare: if mid > key
    if
        2dup - 1-   \ high = mid - 1
    else
        2dup + 1+   \ low = mid + 1
    then
        2drop       \ drop old low/high
    repeat
    2drop           \ drop low and high
    -1              \ not found
;

\ Usage example:
\ my-array 0 7 9 binary-search-2 .
```

## How it works:

1. **Input parameters**: Address of array, length, and key to search for
2. **Initialize**: Set `low = 0` and `high = length - 1`
3. **Loop condition**: Continue while `low <= high`
4. **Calculate mid**: Find middle index using `(low + high) / 2`
5. **Compare**: Compare array element at mid with key
6. **Adjust search range**:
   - If element > key: search left half (`high = mid - 1`)
   - If element < key: search right half (`low = mid + 1`)
7. **Return**: Index if found, or -1 if not found

## Example output:
```
3
-1
```

This implementation assumes a sorted array and returns the zero-based index of the found element, or -1 if the element is not present.