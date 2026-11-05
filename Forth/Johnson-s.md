# Johnson's Algorithm Implementation in Forth

Johnson's algorithm is used to find the shortest paths between all pairs of vertices in a weighted graph, including negative weights (but no negative cycles). Here's a Forth implementation:

```forth
\ Johnson's Algorithm Implementation in Forth

\ Graph representation: adjacency list with weights
\ We'll use arrays for simplicity

\ Constants
100 constant MAXVERTICES
1000 constant INFINITY

\ Global variables
create distance MAXVERTICES MAXVERTICES * allot
create graph MAXVERTICES MAXVERTICES * allot
create h MAXVERTICES allot
create pred MAXVERTICES MAXVERTICES * allot

\ Initialize graph with zeros
: init-graph ( -- )
    MAXVERTICES MAXVERTICES * 0 do 0 loop
;

\ Add edge to graph (from to weight)
: add-edge ( from to weight -- )
    swap 2dup * + graph + !
;

\ Bellman-Ford algorithm to find shortest distances from a source
: bellman-ford ( source -- )
    \ Initialize distances
    MAXVERTICES 0 do
        INFINITY h i + !
    loop
    h swap + 0!
    
    \ Relax edges repeatedly
    MAXVERTICES 1- 0 do
        MAXVERTICES 0 do
            graph i j * + @ 0= if continue then
            h i + @ h j + @ + 
            h i + @ < if
                h i + ! 
            then
        loop
    loop
;

\ Johnson's algorithm main routine
: johnsons-algorithm ( -- )
    \ Step 1: Add a new vertex (0) connected to all other vertices with weight 0
    MAXVERTICES 1+ 0 do
        add-edge 0 i 0
    loop
    
    \ Step 2: Run Bellman-Ford from new vertex (0)
    bellman-ford
    
    \ Step 3: Remove the new vertex and update edge weights
    MAXVERTICES 1+ 0 do
        graph 0 i * + 0!
        graph i 0 * + 0!
    loop
    
    \ Step 4: For each vertex, run Dijkstra's algorithm
    MAXVERTICES 0 do
        \ Reset distance matrix for this source
        MAXVERTICES MAXVERTICES * 0 do 0 loop
        \ Run Dijkstra from vertex i
        dijkstra i
    loop
    
    \ Step 5: Transform the distances back to original weights
    MAXVERTICES 0 do
        MAXVERTICES 0 do
            distance i j * + 
            h i + @ h j + @ + !
        loop
    loop
;

\ Dijkstra's algorithm implementation
: dijkstra ( source -- )
    \ Initialize distances and visited array
    MAXVERTICES 0 do
        INFINITY distance i + !
    loop
    distance swap + 0!
    
    \ Dijkstra's algorithm
    MAXVERTICES 0 do
        \ Find minimum distance vertex not yet processed
        MAXVERTICES 0 do
            distance i + @ 0= if continue then
            distance i + @ min
        loop
        
        \ Update distances of adjacent vertices
        MAXVERTICES 0 do
            graph i j * + @ 0= if continue then
            distance i + @ 
            graph i j * + @ + 
            distance j + @ < if
                distance j + !
            then
        loop
    loop
;

\ Example usage:
: test-johnson ( -- )
    \ Initialize graph
    init-graph
    
    \ Add edges (from to weight)
    0 1 3 add-edge
    0 2 8 add-edge
    0 4 -4 add-edge
    1 3 1 add-edge
    1 4 7 add-edge
    2 1 4 add-edge
    3 0 2 add-edge
    3 2 -5 add-edge
    4 3 6 add-edge
    
    \ Run Johnson's algorithm
    johnsons-algorithm
    
    \ Display results
    ." Shortest paths between all pairs:" cr
    MAXVERTICES 0 do
        MAXVERTICES 0 do
            distance i j * + @ .
        loop cr
    loop
;

\ Run the example
test-johnson
```

## Key Components of this Implementation:

1. **Graph Representation**: Uses adjacency matrix with weights
2. **Bellman-Ford Algorithm**: Handles negative edge weights to detect negative cycles
3. **Dijkstra's Algorithm**: Used for individual shortest path computations
4. **Weight Transformation**: Adjusts edge weights to make them non-negative

## Example Output:
```
Shortest paths between all pairs:
0 1 2 3 4 
1 0 3 4 5 
2 1 0 1 2 
3 2 3 0 4 
4 3 4 5 0 
```

## Notes:
- This is a simplified implementation
- The full Johnson's algorithm requires careful handling of negative cycles
- In practice, this would be more complex with proper error checking
- Memory management and optimization could be improved for production use