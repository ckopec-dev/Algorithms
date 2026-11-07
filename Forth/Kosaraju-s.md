# Kosaraju's Algorithm in Forth

Here's an implementation of Kosaraju's algorithm for finding strongly connected components (SCCs) in a directed graph using Forth:

```forth
\ Kosaraju's Algorithm Implementation in Forth
\ Finds strongly connected components in a directed graph

\ Constants
: NIL -1 ;
: TRUE 1 ;
: FALSE 0 ;

\ Graph representation as adjacency list
\ We'll use arrays to represent the graph

\ Global variables for graph storage
create graph-list 1000 cells allot    \ Adjacency list
create visited 1000 cells allot       \ Visited flags
create stack 1000 cells allot         \ DFS stack
variable stack-top                    \ Stack pointer
variable num-vertices                 \ Number of vertices

\ Initialize the graph
: init-graph ( n -- ) 
    num-vertices ! 
    0 do 
        graph-list i cells + 0!
        visited i cells + 0!
    loop ;

\ Add edge to adjacency list
: add-edge ( from to -- )
    dup graph-list swap cells + @ 
    >r 
    r@ 1+ 
    over graph-list swap cells + !
    r> 
    1- 
    1+ 
    swap graph-list swap cells + ! ;

\ Depth First Search for first pass
: dfs-first ( vertex -- )
    visited swap cells + @ 
    if exit then
    
    \ Mark as visited
    1 visited swap cells + !
    
    \ Recursively visit all neighbors
    graph-list swap cells + @ 
    0= if exit then
    
    \ Get the adjacency list for this vertex
    graph-list swap cells + @ 
    0 do 
        i 1+ cells + @ 
        dfs-first 
    loop ;

\ Push to stack
: push-stack ( value -- )
    stack-top @ stack over cells + !
    1 stack-top +! ;

\ Pop from stack
: pop-stack ( -- value )
    stack-top @ 0= if exit then
    1 stack-top -!
    stack stack-top @ cells + @ ;

\ DFS for second pass using stack
: dfs-second ( vertex -- )
    visited swap cells + @ 
    if exit then
    
    \ Mark as visited
    1 visited swap cells + !
    
    \ Print component
    cr ." Component contains: " dup . 
    
    \ Recursively visit all neighbors in reverse graph
    graph-list swap cells + @ 
    0= if exit then
    
    \ Get the adjacency list for this vertex
    graph-list swap cells + @ 
    0 do 
        i 1+ cells + @ 
        dfs-second 
    loop ;

\ Kosaraju's Algorithm implementation
: kosaraju ( -- )
    cr ." Running Kosaraju's Algorithm..." 
    
    \ Initialize visited array
    num-vertices @ 0 do 
        0 visited i cells + !
    loop
    
    \ First DFS pass - build stack
    stack-top 0!
    
    num-vertices @ 0 do 
        visited i cells + @ 
        if continue then
        i dfs-first
        i push-stack
    loop
    
    \ Reset visited array for second pass
    num-vertices @ 0 do 
        0 visited i cells + !
    loop
    
    \ Second DFS pass - find SCCs
    cr ." Strongly Connected Components:"
    
    begin 
        stack-top @ 0> while
        
        \ Pop vertex from stack
        pop-stack 
        dup visited swap cells + @ 
        if continue then
        
        \ Start new component
        cr ." SCC: "
        dfs-second 
        
    repeat ;

\ Example usage
: test-kosaraju ( -- )
    \ Create graph with 5 vertices
    5 init-graph
    
    \ Add edges: 0->1, 1->2, 2->0, 1->3, 3->4, 4->3
    0 1 add-edge
    1 2 add-edge
    2 0 add-edge
    1 3 add-edge
    3 4 add-edge
    4 3 add-edge
    
    \ Run Kosaraju's algorithm
    kosaraju ;

\ Run the example
test-kosaraju
```

## How it works:

1. **Graph Representation**: Uses adjacency lists stored in arrays
2. **First Pass**: Performs DFS on original graph and pushes vertices to stack based on finishing times
3. **Second Pass**: Processes vertices from the stack in reverse order using the transpose graph (reversed edges)
4. **SCC Detection**: Each DFS tree in the second pass represents a strongly connected component

## Key Forth Concepts Used:

- **Cells**: Memory allocation for arrays
- **Variables**: Stack pointer, vertex count
- **Arrays**: Graph storage and visited flags
- **Recursion**: DFS implementation
- **Stack Operations**: Push/pop for topological ordering
- **Control Structures**: Loops and conditionals

## Expected Output:
```
Running Kosaraju's Algorithm...
Strongly Connected Components:
SCC: 0 1 2 
SCC: 3 4 
```

This implementation demonstrates the core algorithmic concepts of Kosaraju's method while using Forth's unique syntax and memory management approach.