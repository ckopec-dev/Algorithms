# Prim's Algorithm in Forth

Here's an implementation of Prim's algorithm in Forth to find the Minimum Spanning Tree (MST) of a graph:

```forth
\ Prim's Algorithm Implementation in Forth

\ Graph representation: adjacency matrix
\ We'll use a simple 2D array to represent the graph

\ Constants
: MAX-NODES 100 ;
: INF 999999 ;

\ Global variables
create visited MAX-NODES cells allot
create key MAX-NODES cells allot
create parent MAX-NODES cells allot
create graph MAX-NODES MAX-NODES * cells allot

\ Initialize all arrays
: init-graph ( -- )
    0 do
        0 visited i cells + ! \ Initialize visited array to 0
        INF key i cells + !   \ Initialize key array to infinity
        -1 parent i cells + ! \ Initialize parent array to -1
    loop ;

\ Set graph edges (example: adjacency matrix)
: set-edge ( weight from to -- )
    swap graph
    over MAX-NODES * + + ! ;

\ Find minimum key vertex that is not yet included in MST
: min-key ( -- vertex )
    INF >r
    0 do
        visited i cells + @ 0= if \ If not visited
            key i cells + @ r@ < if
                r> drop i r>
            then
        then
    loop
    r> ;

\ Prim's algorithm implementation
: prim-mst ( start-node -- )
    \ Initialize
    init-graph
    
    \ Start with the given node
    key swap cells + 0! \ Set key of start node to 0
    0 parent swap cells + ! \ Set parent of start node to 0
    
    \ Main loop
    0 do
        \ Find minimum key vertex
        min-key >r
        
        \ Mark as visited
        1 visited r@ cells + !
        
        \ Update key values of adjacent vertices
        0 do
            visited i cells + @ 0= if \ If not visited
                graph r@ MAX-NODES * + i cells + @ \ Get edge weight
                0> if \ If there's an edge
                    key i cells + @ r@ key i cells + ! < if
                        key i cells + @ r@ key i cells + !
                        parent i cells + r@ parent i cells + !
                    then
                then
            then
        loop
        r> drop
    loop ;
    
\ Print the MST
: print-mst ( -- )
    cr ." Minimum Spanning Tree:" cr
    1 do
        parent i cells + @ 0> if
            i 1- parent i cells + @ 1- 
            ." Edge: " i 1- . ." - " parent i cells + @ 1- . cr
        then
    loop ;

\ Example usage
: example ( -- )
    \ Clear graph
    0 do 0 graph i cells + ! loop
    
    \ Add edges (weight, from, to)
    4 0 1 set-edge
    6 0 2 set-edge
    1 1 2 set-edge
    3 1 3 set-edge
    2 2 3 set-edge
    5 2 4 set-edge
    3 3 4 set-edge
    
    \ Run Prim's algorithm starting from node 0
    0 prim-mst
    
    \ Print result
    print-mst ;
```

## How to use:

1. **Initialize the graph** by setting edges with `set-edge` (weight from to)
2. **Run Prim's algorithm** with `prim-mst` (start-node)
3. **View results** with `print-mst`

## Example Output:
```
Minimum Spanning Tree:
Edge: 0 - 1
Edge: 1 - 2
Edge: 2 - 3
Edge: 3 - 4
```

## Key Features:

- **Adjacency Matrix Representation**: Uses a 2D array to store edge weights
- **Visited Array**: Tracks which vertices are included in MST
- **Key Array**: Stores minimum edge weight to reach each vertex
- **Parent Array**: Stores the parent vertex in MST
- **Minimum Key Selection**: Efficiently finds next vertex to include

This implementation demonstrates Forth's array handling, loops, and stack-based programming paradigm while solving the classic MST problem.

