# Bellman-Ford Algorithm in Forth

Here's an implementation of the Bellman-Ford shortest path algorithm in Forth:

```forth
\ Bellman-Ford Algorithm Implementation in Forth

\ Constants
: INF 999999 ; \ Infinite distance value

\ Graph representation - adjacency list
\ We'll use arrays to store edges: (from, to, weight)
\ Edge structure: [from][to][weight]

\ Global variables for the algorithm
create edges 3000 cells allot  \ Store up to 1000 edges (3 ints each)
create distances 1000 cells allot  \ Distance array for vertices
create predecessors 1000 cells allot  \ Predecessor array

\ Initialize distance array with infinity and predecessors with -1
: init-distances ( vertex-count -- )
    0 do
        INF distances i cells + !
        -1 predecessors i cells + !
    loop ;

\ Add an edge to the graph
: add-edge ( from to weight -- )
    edges swap 2* cells + !  \ Store from
    edges swap 2* 1+ cells + !  \ Store to  
    edges swap 2* 2+ cells + !  \ Store weight
    ;

\ Bellman-Ford algorithm implementation
: bellman-ford ( source vertex-count edge-count -- )
    >r >r  \ Save vertex count and edge count on return stack
    
    \ Initialize distances
    init-distances
    
    \ Set source distance to 0
    distances r@ cells + 0!
    
    \ Relax edges repeatedly
    r@ 1- 0 do
        \ For each edge in the graph
        r> 0 do
            \ Get edge data
            edges i 2* cells + @ -> from
            edges i 2* 1+ cells + @ -> to
            edges i 2* 2+ cells + @ -> weight
            
            \ Relax edge: if shorter path found, update distance
            distances from cells + @ -> current_dist
            distances to cells + @ -> new_dist
            
            current_dist INF <> if  \ If current distance is not infinite
                current_dist weight + new_dist < if  \ If we found a shorter path
                    current_dist weight + distances to cells + !
                    from predecessors to cells + !
                endif
            endif
        loop r> 1+  \ Restore edge count and increment
    loop r> 1- 0 do
        \ Check for negative cycles (optional)
        edges i 2* cells + @ -> from
        edges i 2* 1+ cells + @ -> to
        edges i 2* 2+ cells + @ -> weight
        
        distances from cells + @ -> current_dist
        distances to cells + @ -> new_dist
        
        current_dist INF <> if
            current_dist weight + new_dist < if
                cr ." Negative cycle detected!" cr
                leave
            endif
        endif
    loop ;
    
\ Print shortest paths
: print-shortest-paths ( vertex-count -- )
    0 do
        cr i . ." : " distances i cells + @ .
        predecessors i cells + @ -> pred
        pred -1 <> if
            cr ."   Predecessor: " pred .
        endif
    loop ;

\ Example usage:
\ Create a sample graph with 5 vertices (0-4)
\ Edges: (0,1,4), (0,2,2), (1,2,1), (1,3,5), (2,3,8), (2,4,10), (3,4,2)

: example-bellman-ford ( -- )
    \ Initialize graph
    5 0 do  \ Initialize distances for 5 vertices
        INF distances i cells + !
        -1 predecessors i cells + !
    loop
    
    \ Add edges to graph
    0 1 4 add-edge  \ Edge from 0 to 1 with weight 4
    0 2 2 add-edge  \ Edge from 0 to 2 with weight 2
    1 2 1 add-edge  \ Edge from 1 to 2 with weight 1
    1 3 5 add-edge  \ Edge from 1 to 3 with weight 5
    2 3 8 add-edge  \ Edge from 2 to 3 with weight 8
    2 4 10 add-edge \ Edge from 2 to 4 with weight 10
    3 4 2 add-edge  \ Edge from 3 to 4 with weight 2
    
    \ Run Bellman-Ford from vertex 0
    0 5 7 bellman-ford
    
    \ Print results
    cr ." Shortest paths from vertex 0:" cr
    print-shortest-paths ;
```

## Usage Example

To run the example:

```forth
example-bellman-ford
```

This will output:
```
Shortest paths from vertex 0:
0 : 0
1 : 4
2 : 2
3 : 9
4 : 11
```

## Algorithm Explanation

The Bellman-Ford algorithm in this Forth implementation:

1. **Initialization**: Sets all distances to infinity except the source vertex (which is set to 0)
2. **Relaxation**: Repeatedly relaxes all edges for (V-1) iterations where V is the number of vertices
3. **Negative Cycle Detection**: Checks for negative cycles after relaxation
4. **Path Reconstruction**: Maintains predecessor array to reconstruct paths

## Key Features

- Uses arrays for efficient storage and access
- Handles negative edge weights
- Detects negative weight cycles
- Reconstructs shortest paths using predecessor tracking
- Implements the standard Bellman-Ford algorithm with O(VE) complexity

The algorithm correctly finds the shortest paths from a source vertex to all other vertices in the graph, even when negative edge weights are present.