# Dijkstra's Algorithm in Forth

Here's an implementation of Dijkstra's algorithm in Forth for finding the shortest path in a graph:

```forth
\ Dijkstra's Algorithm Implementation in Forth

\ Graph representation: adjacency list with weights
\ We'll use a simple array-based approach

\ Constants
100 CONSTANT MAX-VERTICES
1000 CONSTANT INFINITY

\ Global variables
CREATE distances MAX-VERTICES cells ALLOT
CREATE visited MAX-VERTICES ALLOT
CREATE graph MAX-VERTICES MAX-VERTICES * ALLOT

\ Initialize graph
: init-graph ( -- )
    MAX-VERTICES 0 DO
        MAX-VERTICES 0 DO
            graph i MAX-VERTICES * j + ! 0
        LOOP
    LOOP ;

\ Add edge to graph (from to weight)
: add-edge ( from to weight -- )
    graph swap MAX-VERTICES * + ! ;

\ Initialize distances array
: init-distances ( start -- )
    MAX-VERTICES 0 DO
        distances i cells + 0 !
    LOOP
    distances swap cells + INFINITY ! ;

\ Find minimum distance vertex not yet visited
: min-distance ( -- vertex )
    MAX-VERTICES 0 DO
        visited i + @ 0= 
        IF
            distances i cells + @
            dup distances 0 cells + @ < 
            IF
                distances 0 cells + !
                i
            ELSE
                drop
            THEN
        THEN
    LOOP ;

\ Dijkstra's algorithm
: dijkstra ( start -- )
    init-distances
    MAX-VERTICES 0 DO
        min-distance
        dup visited + 1 swap visited + !
        MAX-VERTICES 0 DO
            graph swap MAX-VERTICES * + @ 0> 
            IF
                distances swap cells + @
                graph swap MAX-VERTICES * + @ +
                distances i cells + @
                < IF
                    distances i cells + !
                THEN
            THEN
        LOOP
    LOOP ;

\ Print distances
: print-distances ( -- )
    MAX-VERTICES 0 DO
        distances i cells + @
        i 0 .r ." : " . cr
    LOOP ;

\ Example usage
: example ( -- )
    init-graph
    \ Create graph: 0->1(4), 0->7(8), 1->2(8), 1->7(11)
    \              2->3(7), 2->8(2), 2->5(4), 3->4(9)
    \              3->5(14), 4->5(10), 5->6(2), 6->7(1), 6->8(6)
    \              7->8(7)
    0 1 4 add-edge
    0 7 8 add-edge
    1 2 8 add-edge
    1 7 11 add-edge
    2 3 7 add-edge
    2 8 2 add-edge
    2 5 4 add-edge
    3 4 9 add-edge
    3 5 14 add-edge
    4 5 10 add-edge
    5 6 2 add-edge
    6 7 1 add-edge
    6 8 6 add-edge
    7 8 7 add-edge
    
    \ Run Dijkstra from vertex 0
    0 dijkstra
    
    \ Print results
    ." Shortest distances from vertex 0:" cr
    print-distances ;

\ Run the example
example
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses an adjacency matrix to store edge weights
2. **Memory Management**: Allocates arrays for distances, visited flags, and graph edges
3. **Core Algorithm**: Implements the standard Dijkstra's algorithm steps:
   - Initialize distances to infinity except start vertex
   - Select minimum distance unvisited vertex
   - Update distances of adjacent vertices
   - Repeat until all vertices processed

## Usage Notes:

- The example creates a sample graph with 9 vertices
- `add-edge` function adds weighted edges between vertices
- `dijkstra` function computes shortest paths from a given start vertex
- Results are printed showing shortest distances from the start vertex to all other vertices

This implementation demonstrates Forth's stack-based approach to algorithmic problems while maintaining the core logic of Dijkstra's shortest path algorithm.

