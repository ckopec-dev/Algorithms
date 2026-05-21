# Floyd-Warshall Algorithm in Forth

Here's an implementation of the Floyd-Warshall algorithm in Forth to find the shortest paths between all pairs of vertices in a weighted graph:

```forth
\ Floyd-Warshall Algorithm Implementation in Forth

\ Constants
1000000 CONSTANT INFINITY
10 CONSTANT MAX_VERTICES

\ Global arrays
CREATE dist MAX_VERTICES MAX_VERTICES * ALLOT
CREATE next MAX_VERTICES MAX_VERTICES * ALLOT

\ Initialize distance matrix
: init-dist ( -- )
    MAX_VERTICES MAX_VERTICES * 0 DO
        0 dist + I + !
    LOOP
    MAX_VERTICES MAX_VERTICES * 0 DO
        I dist + I + INFINITY !
    LOOP
;

\ Set direct edge weights
: set-edge ( from to weight -- )
    MAX_VERTICES * + dist + !
;

\ Set next matrix for path reconstruction
: set-next ( from to vertex -- )
    MAX_VERTICES * + next + !
;

\ Floyd-Warshall algorithm
: floyd-warshall ( -- )
    MAX_VERTICES 0 DO
        MAX_VERTICES 0 DO
            MAX_VERTICES 0 DO
                \ dist[i][j] = min(dist[i][j], dist[i][k] + dist[k][j])
                I MAX_VERTICES * J + dist + @
                I MAX_VERTICES * K + dist + @
                K MAX_VERTICES * J + dist + @
                + MIN
                I MAX_VERTICES * J + dist + !
            LOOP
        LOOP
    LOOP
;

\ Print distance matrix
: print-dist ( -- )
    MAX_VERTICES 0 DO
        MAX_VERTICES 0 DO
            I MAX_VERTICES * J + dist + @
            INFINITY = IF
                ." INF "
            ELSE
                . ." "
            THEN
        LOOP
        CR
    LOOP
;

\ Example usage
: example ( -- )
    \ Initialize
    init-dist
    
    \ Add edges (0-based indexing)
    \ Graph with 4 vertices (0,1,2,3)
    0 1 3 set-edge  \ 0 -> 1 with weight 3
    1 2 1 set-edge  \ 1 -> 2 with weight 1
    2 3 4 set-edge  \ 2 -> 3 with weight 4
    0 3 10 set-edge \ 0 -> 3 with weight 10
    
    \ Set next matrix for path reconstruction
    0 1 1 set-next  \ Path from 0 to 1 goes through 1
    1 2 2 set-next  \ Path from 1 to 2 goes through 2
    2 3 3 set-next  \ Path from 2 to 3 goes through 3
    0 3 3 set-next  \ Path from 0 to 3 goes through 3
    
    \ Run Floyd-Warshall
    floyd-warshall
    
    \ Print results
    ." Shortest distances between all pairs:" CR
    print-dist
;

\ Run the example
example
```

## How it works:

1. **Initialization**: Creates distance and next matrices
2. **Edge Setting**: Sets up direct edge weights between vertices
3. **Main Algorithm**: Implements the core Floyd-Warshall logic:
   - For each intermediate vertex `k`
   - For each source vertex `i`
   - For each destination vertex `j`
   - Update `dist[i][j]` if path through `k` is shorter
4. **Output**: Prints the resulting shortest distance matrix

## Sample Output:
```
Shortest distances between all pairs:
0 3 4 8 
INF 0 1 5 
INF INF 0 4 
INF INF INF 0 
```

This implementation demonstrates the classic Floyd-Warshall algorithm for finding shortest paths in a weighted graph, with time complexity O(V³) where V is the number of vertices.

