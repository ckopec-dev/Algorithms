# Borůvka's Algorithm in Forth

Here's an implementation of Borůvka's algorithm for finding Minimum Spanning Tree (MST) in Forth:

```forth
\ Borůvka's Algorithm Implementation in Forth

\ Graph representation: adjacency list with edge weights
\ We'll use a simple array-based approach

\ Constants
100 CONSTANT MAX_VERTICES
1000 CONSTANT MAX_EDGES
1000000 CONSTANT INF

\ Global variables
CREATE edges MAX_EDGES 3 * ALLOT
CREATE parent MAX_VERTICES ALLOT
CREATE rank MAX_VERTICES ALLOT
CREATE min_edge MAX_VERTICES ALLOT
CREATE visited MAX_VERTICES ALLOT

\ Initialize disjoint set structure
: make-set ( vertex -- )
    parent SWAP ! 0 rank SWAP ! ;

\ Find root with path compression
: find ( vertex -- root )
    parent SWAP @ dup 0< IF
        drop SWAP
    ELSE
        find
        parent SWAP ! \ Path compression
    THEN ;

\ Union by rank
: union ( vertex1 vertex2 -- )
    find SWAP find SWAP
    SWAP = IF EXIT THEN \ Same set
    rank SWAP @ SWAP @ > IF
        parent SWAP ! \ Make smaller rank tree a subtree
    ELSE
        parent SWAP ! \ Make smaller rank tree a subtree
        rank SWAP @ SWAP @ 1+ rank SWAP !
    THEN ;

\ Initialize all vertices
: init-graph ( vertex_count -- )
    0 DO
        I make-set
        0 min_edge I 3 * + !
        0 visited I + 0!
    LOOP ;

\ Add edge to graph
: add-edge ( weight from to -- )
    edges SWAP 3 * + !  \ weight
    edges SWAP 3 * 1+ + !  \ from
    edges SWAP 3 * 2+ + !  \ to
    1+ ;

\ Borůvka's algorithm implementation
: boruvka-mst ( vertex_count edge_count -- )
    init-graph
    0 DO
        \ Initialize minimum edge for each component
        0 DO
            0 min_edge I 3 * + !
        LOOP
        
        \ Find minimum edge for each component
        0 DO
            edges I 3 * + @  \ weight
            edges I 3 * 1+ + @  \ from
            edges I 3 * 2+ + @  \ to
            
            find SWAP find SWAP
            SWAP = IF 0 0 0 0 EXIT THEN \ Same component
            
            \ Check if this edge is minimum for the component
            min_edge SWAP 3 * + @ 0= IF
                \ First edge for this component
                I min_edge SWAP 3 * + !
            ELSE
                min_edge SWAP 3 * + @ > IF
                    \ Update minimum edge
                    I min_edge SWAP 3 * + !
                THEN
            THEN
        LOOP
        
        \ Add minimum edges to MST
        0 DO
            min_edge I 3 * + @ 0> IF
                \ Add edge to MST
                min_edge I 3 * + @  \ weight
                edges I 3 * 1+ + @  \ from
                edges I 3 * 2+ + @  \ to
                \ Print edge (for demonstration)
                ." Edge: " I 3 * 1+ @ . ." - " I 3 * 2+ @ . ." Weight: " I 3 * + @ . CR
                \ Union the components
                find SWAP find SWAP
                SWAP = IF 0 0 0 0 EXIT THEN
                union
            THEN
        LOOP
        
        \ Check if we have a spanning tree
        1 DO
            find 0= IF
                \ All vertices in same component
                0 0 0 0 EXIT
            THEN
        LOOP
        
    LOOP ;

\ Example usage
: test-boruvka ( -- )
    \ Create a graph with 4 vertices
    4 \ vertex count
    5 \ edge count
    
    \ Add edges (weight, from, to)
    10 0 1 add-edge
    6 0 2 add-edge
    5 0 3 add-edge
    15 1 2 add-edge
    4 2 3 add-edge
    
    \ Run Borůvka's algorithm
    .( Running Borůvka's Algorithm:) CR
    boruvka-mst ;

\ Alternative simpler implementation for demonstration
: simple-boruvka ( -- )
    \ Simple example with 4 vertices
    4 init-graph
    
    \ Example edges
    10 0 1 \ Edge 0-1 weight 10
    6 0 2 \ Edge 0-2 weight 6
    5 0 3 \ Edge 0-3 weight 5
    15 1 2 \ Edge 1-2 weight 15
    4 2 3 \ Edge 2-3 weight 4
    
    \ Print the edges
    .( Graph edges:) CR
    0 5 DO
        edges I 3 * + @ . 
        edges I 3 * 1+ + @ . 
        edges I 3 * 2+ + @ . 
        ."  " CR
    LOOP ;

\ Run the test
test-boruvka
```

## Key Features of this Implementation:

1. **Disjoint Set Data Structure**: Uses `find` and `union` operations with path compression and union by rank
2. **Graph Representation**: Simple array-based edge storage with weight, from, and to vertices
3. **Borůvka's Logic**: 
   - For each component, finds the minimum weight edge connecting it to another component
   - Adds these edges to the MST
   - Repeats until all vertices are connected

## Algorithm Steps:
1. Initialize each vertex as its own component
2. For each component, find the minimum weight edge to another component
3. Add these minimum edges to the MST
4. Union the components connected by these edges
5. Repeat until all vertices are in one component

## Time Complexity:
- O(E log V) where E is edges and V is vertices
- More efficient than Kruskal's or Prim's for sparse graphs

This implementation demonstrates the core concepts of Borůvka's algorithm in Forth, though it's simplified for clarity and readability in the Forth environment.

