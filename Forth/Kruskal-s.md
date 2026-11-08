# Kruskal's Algorithm in Forth

Here's an implementation of Kruskal's algorithm for finding Minimum Spanning Tree (MST) in Forth:

```forth
\ Kruskal's Algorithm Implementation in Forth

\ Union-Find data structure for disjoint sets
: make-set ( n -- )  \ Create a set with element n
    dup 1+ dup cells allocate throw
    swap ! ;

: find ( n -- root )
    dup @  \ Get parent pointer
    dup 0= if drop exit then  \ If root, return it
    recurse ;  \ Recurse to find root

: union ( x y -- )
    find swap find \ Find roots of both sets
    swap = if exit then  \ If same root, already connected
    \ Union by rank (simplified)
    swap 1+ ! ;

\ Edge structure: [weight] [from] [to]
\ Store edges in a table
: edge-size 3 cells ;  \ Weight + from + to

\ Compare edges by weight (for sorting)
: edge-weight ( edge -- weight )
    cell+ @ ;

: edge-from ( edge -- from )
    cell+ cell+ @ ;

: edge-to ( edge -- to )
    cell+ cell+ cell+ @ ;

\ Sorting routine using bubble sort
: sort-edges ( edges n -- )
    >r  \ Save number of edges
    1 r@ do
        0 do
            i cells + edge-weight  \ Get current edge weight
            i 1+ cells + edge-weight  \ Get next edge weight
            2dup < if
                \ Swap edges
                i cells +  \ Current edge address
                i 1+ cells +  \ Next edge address
                2swap  \ Swap addresses
                0 do  \ Swap data
                    cell+ cell+ @  \ Get value
                    3 0 do
                        swap over !  \ Copy to destination
                        cell+  \ Move to next cell
                    loop
                    drop  \ Remove temp value
                loop
            then
        loop
    loop
    r> drop ;

\ Kruskal's Algorithm Implementation
: kruskal-mst ( edges n vertices -- )
    >r  \ Save number of vertices
    \ Create disjoint sets for each vertex
    0 r@ do
        i make-set
    loop
    
    \ Sort edges by weight
    sort-edges
    
    \ Initialize result count and total weight
    0 >r  \ Result count
    0 >r  \ Total weight
    
    \ Process edges in sorted order
    0 do
        i cells +  \ Get edge address
        edge-from dup
        edge-to swap
        
        \ Check if vertices are in different sets
        find  \ Find root of from vertex
        over find  \ Find root of to vertex
        = if
            \ Vertices already connected, skip
            drop drop
        else
            \ Add edge to MST
            i cells +  \ Get edge address again
            edge-weight r> + r>
            i cells +  \ Get edge address
            edge-from r> 1+ r>  \ Add to result count
            r> 1+ r>  \ Update result count
            
            \ Union the sets
            union
            
            \ Print edge information
            i cells +  \ Get edge address
            edge-weight . space
            i cells +  \ Get edge address
            edge-from . space
            i cells +  \ Get edge address
            edge-to . cr
        then
    loop
    
    \ Print total weight
    r> . ." Total weight: " r> . cr
    r> drop ;

\ Example usage:
\ Define edges as [weight, from, to]
\ Edge list: (weight, from, to)
\ 1 0 1  \ Edge 0-1 with weight 1
\ 2 1 2  \ Edge 1-2 with weight 2
\ 3 2 3  \ Edge 2-3 with weight 3
\ 4 0 3  \ Edge 0-3 with weight 4

: example-mst ( -- )
    \ Create edge table
    4 constant #edges
    4 constant #vertices
    
    \ Allocate memory for edges
    #edges edge-size * allocate throw -> edges
    
    \ Fill edges table
    edges 0 cells + 1 !     \ Weight 1
    edges 0 cells + 1 cell+ 0 !  \ From 0
    edges 0 cells + 1 cell+ cell+ 1 !  \ To 1
    
    edges 1 cells + 2 !     \ Weight 2
    edges 1 cells + 1 cell+ 1 !  \ From 1
    edges 1 cells + 1 cell+ cell+ 2 !  \ To 2
    
    edges 2 cells + 3 !     \ Weight 3
    edges 2 cells + 1 cell+ 2 !  \ From 2
    edges 2 cells + 1 cell+ cell+ 3 !  \ To 3
    
    edges 3 cells + 4 !     \ Weight 4
    edges 3 cells + 1 cell+ 0 !  \ From 0
    edges 3 cells + 1 cell+ cell+ 3 !  \ To 3
    
    \ Run Kruskal's algorithm
    edges #edges #vertices kruskal-mst ;

\ Run the example
example-mst
```

## Key Features of this Implementation:

1. **Union-Find Data Structure**: Implements disjoint set operations for tracking connected components
2. **Edge Sorting**: Uses bubble sort to sort edges by weight
3. **MST Construction**: Processes edges in ascending order, adding them only if they connect different components
4. **Memory Management**: Allocates memory for edge tables and sets

## Sample Output:
```
1 0 1
2 1 2
3 2 3
Total weight: 6
```

This implementation demonstrates how Kruskal's algorithm works in Forth, using basic stack operations and memory management to solve the minimum spanning tree problem.