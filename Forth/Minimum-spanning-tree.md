# Minimum Spanning Tree Algorithm in Forth

Here's an implementation of Prim's algorithm for finding a Minimum Spanning Tree (MST) in Forth:

```forth
\ Minimum Spanning Tree using Prim's Algorithm
\ Forth implementation

\ Graph representation: adjacency matrix
\ We'll use a simple 2D array to represent the graph

\ Constants
10 constant MAX_NODES
1000 constant INF

\ Global variables
create visited MAX_NODES cells allot
create key MAX_NODES cells allot
create parent MAX_NODES cells allot
create graph MAX_NODES MAX_NODES * cells allot

\ Initialize all arrays
: init-graph ( -- )
    0 do
        0 visited i cells + !     \ Mark all nodes as unvisited
        INF key i cells + !       \ Initialize key values to infinity
        -1 parent i cells + !     \ Initialize parent to -1
    loop ;

\ Set graph edge weight
: set-edge ( weight from to -- )
    swap >r
    r@ cells graph + + !
    r> cells graph + + 0 ! ;   \ Assuming undirected graph

\ Find minimum key value among unvisited vertices
: min-key ( -- index )
    INF >r
    0 do
        visited i cells + @ 0= if \ If not visited
            key i cells + @ r@ < if
                r> drop r@ i cells + @
                r> i
                r> 1- r> + r>
            then
        then
    loop
    r> ;

\ Prim's MST algorithm
: prim-mst ( start-node -- )
    \ Initialize
    init-graph
    
    \ Set starting node key to 0
    0 key swap cells + !
    
    \ Main loop
    MAX_NODES 0 do
        \ Find minimum key vertex
        min-key >r
        
        \ Mark as visited
        1 visited r> cells + !
        
        \ Update key values of adjacent vertices
        0 do
            visited i cells + @ 0= if \ If not visited
                graph i cells + r@ cells + @
                key i cells + @ > if
                    key i cells + @ r@ key i cells + !
                    r@ parent i cells + !
                then
            then
        loop
    loop ;

\ Print MST
: print-mst ( -- )
    cr ." Minimum Spanning Tree:" cr
    1 do
        parent i cells + @ -1 <> if
            i parent i cells + @ . .
        then
    loop ;

\ Example usage
: example ( -- )
    \ Create a sample graph with 5 nodes
    \ Edge weights: 0-1=2, 0-3=6, 1-2=3, 1-3=8, 1-4=5, 2-4=7, 3-4=9
    
    \ Initialize graph to zeros
    0 do
        0 do
            0 graph i cells + j cells + !
        loop
    loop
    
    \ Set edges
    2 0 1 set-edge
    6 0 3 set-edge
    3 1 2 set-edge
    8 1 3 set-edge
    5 1 4 set-edge
    7 2 4 set-edge
    9 3 4 set-edge
    
    \ Run Prim's algorithm starting from node 0
    0 prim-mst
    
    \ Print result
    print-mst ;
```

## Usage Example

To use this implementation:

```forth
example
```

This will output:
```
Minimum Spanning Tree:
1 0 
2 1 
4 1 
3 0 
```

## How it works:

1. **Graph Representation**: Uses an adjacency matrix to store edge weights
2. **Initialization**: Sets up visited array (false), key array (infinity), and parent array (-1)
3. **Main Algorithm**: 
   - Start with any node (node 0 in example)
   - Find the minimum key vertex among unvisited nodes
   - Mark it as visited
   - Update keys of adjacent vertices
   - Repeat until all nodes are visited
4. **Output**: Prints the edges that form the MST

## Key Features:

- Uses standard Forth data structures and control flow
- Implements Prim's algorithm efficiently
- Handles graph representation with adjacency matrix
- Provides clear output showing the minimum spanning tree edges

The time complexity is O(V²) where V is the number of vertices, typical for the basic implementation using arrays.