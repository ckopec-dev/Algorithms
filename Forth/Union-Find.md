# Union-Find Algorithm in Forth

Here's an implementation of the Union-Find (Disjoint Set) data structure in Forth:

```forth
\ Union-Find Data Structure Implementation

\ Define the size of our union-find structure
1000 CONSTANT MAX-SETS

\ Arrays to store parent and rank information
CREATE PARENT MAX-SETS ALLOT
CREATE RANK  MAX-SETS ALLOT

\ Initialize all elements to be their own parent
: MAKE-SET ( n -- )
    DUP PARENT + !  \ Set parent to itself
    0 RANK + !      \ Set rank to 0
;

\ Initialize all sets from 0 to n-1
: INIT-SETS ( n -- )
    0 DO
        I MAKE-SET
    LOOP
;

\ Find root with path compression
: FIND ( x -- root )
    DUP PARENT + @  \ Get parent of x
    DUP 0< IF      \ If parent is negative, we're at root
        DROP
    ELSE           \ Otherwise, continue searching
        RECURSE    \ Recursive call to find root
        DUP PARENT + !  \ Path compression: point x directly to root
    THEN
;

\ Union two sets by rank
: UNION ( x y -- )
    FIND SWAP FIND  \ Find roots of both elements
    SWAP            \ Put root of x on top
    DUP RANK + @    \ Get rank of root x
    SWAP DUP RANK + @  \ Get rank of root y
    > IF            \ If rank x > rank y
        SWAP PARENT + !  \ Make root y point to root x
    ELSE            \ Otherwise
        SWAP          \ Put root y on top
        DUP PARENT + !  \ Make root x point to root y
        SWAP RANK + @  \ Get rank of root x
        IF           \ If ranks were equal
            1+ RANK + !  \ Increment rank of root y
        THEN
    THEN
;

\ Check if two elements are in the same set
: SAME-SET? ( x y -- flag )
    FIND SWAP FIND =  \ Compare roots
;

\ Example usage:
\ Initialize 10 sets
10 INIT-SETS

\ Create some unions
0 1 UNION
2 3 UNION
4 5 UNION
6 7 UNION
8 9 UNION

\ Union some more
1 2 UNION
3 4 UNION
5 6 UNION
7 8 UNION

\ Test if elements are in same set
0 2 SAME-SET? .  \ Should print 1 (true)
0 3 SAME-SET? .  \ Should print 1 (true)
0 4 SAME-SET? .  \ Should print 0 (false)

\ Find root of element 9
9 FIND .         \ Should print 8 (root of set containing 9)
```

## Key Features:

1. **MAKE-SET**: Creates a new set with one element
2. **FIND**: Finds the root of a set with path compression optimization
3. **UNION**: Merges two sets using union by rank optimization
4. **SAME-SET?**: Checks if two elements belong to the same set

## Time Complexity:
- Find: O(α(n)) where α is the inverse Ackermann function (practically constant)
- Union: O(α(n))
- Space: O(n)

This implementation uses path compression in FIND and union by rank in UNION for optimal performance.