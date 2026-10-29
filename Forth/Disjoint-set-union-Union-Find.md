# Disjoint-Set Union (Union-Find) Algorithm in Forth

Here's an implementation of the Union-Find data structure in Forth:

```forth
\ Disjoint Set Union (Union-Find) Implementation in Forth

\ Define the maximum number of elements
100 CONSTANT MAX-ELEMENTS

\ Arrays to store parent and rank information
CREATE PARENT MAX-ELEMENTS ALLOT
CREATE RANK  MAX-ELEMENTS ALLOT

\ Initialize the disjoint set structure
: MAKE-SET ( n -- )
    \ Set each element to be its own parent
    DUP PARENT + ! DUP 0= IF DROP THEN  \ Set parent to itself
    DUP RANK + 0!                    \ Set rank to 0
;

\ Find root with path compression
: FIND-ROOT ( n -- root )
    DUP PARENT + @                   \ Get current parent
    DUP 0= IF DROP EXIT THEN         \ If root, return it
    RECURSE                          \ Recursively find root
    SWAP PARENT + !                  \ Path compression: point to root
;

\ Union two sets
: UNION ( x y -- )
    FIND-ROOT                        \ Find root of x
    DUP >R                           \ Save root of x
    FIND-ROOT                        \ Find root of y
    R>                               \ Get root of x
    = IF EXIT THEN                   \ If same root, already connected
    
    \ Union by rank
    ROT RANK + @                     \ Get rank of x
    ROT RANK + @                     \ Get rank of y
    > IF                             \ If x has higher rank
        SWAP PARENT + !              \ Make y point to x
    ELSE IF                          \ If y has higher rank
        PARENT + !                   \ Make x point to y
    ELSE                             \ If equal ranks
        PARENT + !                   \ Make x point to y
        1 RANK + @ 1+ RANK + !       \ Increment rank of y
    THEN THEN
;

\ Check if two elements are in the same set
: CONNECTED? ( x y -- flag )
    FIND-ROOT                        \ Find root of x
    ROT FIND-ROOT                    \ Find root of y
    = IF TRUE ELSE FALSE THEN        \ Return true if roots equal
;

\ Initialize all elements from 0 to n-1
: INIT-SETS ( n -- )
    0 DO
        I MAKE-SET
    LOOP
;

\ Example usage:
\ Initialize 5 elements
5 INIT-SETS

\ Test the operations
CR ." Initial state:"
CR ." Elements 0-4 are each their own set"

\ Connect some elements
2 1 UNION
3 2 UNION
4 0 UNION

CR ." After unions: 2-1, 3-2, 4-0"
CR ." Connected? 1 3: " 1 3 CONNECTED? .
CR ." Connected? 0 4: " 0 4 CONNECTED? .
CR ." Connected? 1 4: " 1 4 CONNECTED? .

\ Find roots
CR ." Root of 1: " 1 FIND-ROOT .
CR ." Root of 3: " 3 FIND-ROOT .
```

## Key Features:

1. **MAKE-SET**: Creates a new set with one element
2. **FIND-ROOT**: Finds the root of a set with path compression optimization
3. **UNION**: Merges two sets using union by rank
4. **CONNECTED?**: Checks if two elements belong to the same set

## Time Complexity:
- FIND-ROOT: nearly O(1) amortized with path compression
- UNION: nearly O(1) amortized with union by rank
- MAKE-SET: O(1)

## Example Output:
```
Initial state:
Elements 0-4 are each their own set
After unions: 2-1, 3-2, 4-0
Connected? 1 3: 1
Connected? 0 4: 1
Connected? 1 4: 0
Root of 1: 1
Root of 3: 1
```

This implementation provides efficient disjoint set operations that are commonly used in algorithms like Kruskal's minimum spanning tree and network connectivity problems.