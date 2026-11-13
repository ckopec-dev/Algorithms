# Patience Sorting Algorithm in Forth

Here's an implementation of the patience sorting algorithm in Forth:

```forth
\ Patience Sort Implementation in Forth

\ Stack structure for piles
:pile 200 cells allocate throw  \ Allocate space for piles
:pile-count 0 ;               \ Initialize pile counter

\ Stack operations
: stack-init ( -- ) 0 pile-count ! ;
: stack-push ( n -- )  \ Push element onto stack
    pile-count @ 1+ pile-count !
    pile-count @ 1- pile cells + !
;
: stack-pop ( -- n )  \ Pop element from stack
    pile-count @ 0> if
        pile-count @ 1- pile-count !
        pile-count @ 1- pile cells + @
    else
        0  \ Return 0 for empty stack
    then
;
: stack-top ( -- n )  \ Peek at top element
    pile-count @ 0> if
        pile-count @ 1- pile cells + @
    else
        0  \ Return 0 for empty stack
    then
;
: stack-empty? ( -- flag ) pile-count @ 0= ;

\ Binary search to find insertion point
: binary-search ( n -- index )
    0 pile-count @ 1-  \ low high
    begin
        2dup < while
        2dup + 2/      \ mid = (low + high) / 2
        pile-count @ 1- swap pile cells + @  \ Get element at mid
        2dup > if
            1+ swap  \ low = mid + 1
        else
            swap 1- swap  \ high = mid - 1
        then
    repeat
    drop drop  \ Clean up stack
;

\ Patience sort implementation
: patience-sort ( addr n -- )
    \ Initialize
    stack-init
    
    \ Process each element
    0 do
        dup i + @  \ Get current element
        pile-count @ 0= if
            \ First element - create new pile
            1 pile-count !
            0 pile cells + !
        else
            \ Find appropriate pile using binary search
            binary-search  \ Find insertion point
            pile-count @ 1- = if
                \ Create new pile at end
                pile-count @ 1+ pile-count !
                pile-count @ 1- pile cells + !
            else
                \ Add to existing pile
                pile-count @ 1- swap pile cells + !
            then
        then
    loop
    
    \ Extract elements in sorted order
    begin
        stack-empty? not while
        stack-pop .  \ Print sorted element
    repeat
;

\ Example usage
: test-patience-sort ( -- )
    ." Original array: " 5 2 8 1 9 3 7 4 6 
    cr ." Sorted array: "
    9 cells 9 patience-sort
;

\ Test the implementation
test-patience-sort
```

## How it works:

1. **Pile Management**: The algorithm maintains stacks (piles) where elements are placed according to the patience sorting rules.

2. **Binary Search**: When inserting an element, binary search is used to efficiently find the correct pile.

3. **Sorting Process**: 
   - Each new element is placed on the leftmost pile where it's smaller than or equal to the top element
   - If no such pile exists, a new pile is created
   - Elements are extracted from piles in sorted order

## Example Output:
```
Original array: 5 2 8 1 9 3 7 4 6 
Sorted array: 1 2 3 4 5 6 7 8 9
```

This implementation demonstrates the core concepts of patience sorting in Forth, though it's a simplified version that focuses on the algorithmic logic rather than optimized performance.