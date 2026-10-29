# Depth-First Search (DFS) in Forth

Here's an implementation of the Depth-First Search algorithm in Forth:

```forth
\ DFS Implementation in Forth

\ Graph representation using adjacency list
\ We'll use a simple array-based approach for demonstration

\ Constants
100 CONSTANT MAX_NODES
100 CONSTANT MAX_EDGES

\ Arrays to store graph data
CREATE adj-list MAX_NODES 1+ ALLOT
CREATE visited MAX_NODES 1+ ALLOT
CREATE stack MAX_NODES 1+ ALLOT

\ Stack operations
VARIABLE stack-top

: stack-init ( -- )
    0 stack-top ! ;

: stack-push ( node -- )
    stack-top @ stack +!
    stack-top @ 1- stack + ! ;

: stack-pop ( -- node )
    stack-top @ 0> IF
        stack-top @ 1- stack-top !
        stack-top @ stack + @
    ELSE
        -1 \ Error indicator
    THEN ;

: stack-empty? ( -- flag )
    stack-top @ 0= ;

\ Initialize graph data structures
: graph-init ( -- )
    0 DO
        0 adj-list I + !
        0 visited I + !
    LOOP
    stack-init ;

\ Add edge to adjacency list (undirected graph)
: add-edge ( src dest -- )
    \ Store destination in adjacency list
    adj-list swap + @ 1+ adj-list swap + ! ;

\ DFS recursive implementation
: dfs-recursive ( node -- )
    visited swap + @ 0= IF
        \ Mark node as visited
        1 visited swap + !
        \ Print the node (for demonstration)
        cr ." Visiting node: " swap .
        
        \ Visit all adjacent nodes
        adj-list swap + @ 0 DO
            stack-push I
        LOOP
        
        \ Recursively visit unvisited neighbors
        BEGIN
            stack-empty? 0= WHILE
            stack-pop dfs-recursive
        REPEAT
    THEN ;

\ Iterative DFS implementation using explicit stack
: dfs-iterative ( start-node -- )
    \ Reset visited array
    0 DO
        0 visited I + !
    LOOP
    
    \ Push starting node to stack
    stack-init
    stack-push swap
    
    BEGIN
        stack-empty? 0= WHILE
        \ Pop node from stack
        stack-pop -> node
        
        \ If not visited, process it
        visited node + @ 0= IF
            \ Mark as visited
            1 visited node + !
            
            \ Print node (for demonstration)
            cr ." Visiting node: " node .
            
            \ Push all unvisited neighbors to stack
            adj-list node + @ 0 DO
                visited I + @ 0= IF
                    stack-push I
                THEN
            LOOP
        THEN
    REPEAT ;

\ Example usage
: test-dfs ( -- )
    cr ." Initializing graph..."
    graph-init
    
    \ Create sample graph:
    \ 0 -> 1, 2
    \ 1 -> 0, 3, 4
    \ 2 -> 0, 4
    \ 3 -> 1
    \ 4 -> 1, 2
    
    1 0 add-edge
    2 0 add-edge
    3 1 add-edge
    4 1 add-edge
    4 2 add-edge
    3 1 add-edge
    
    cr ." Starting DFS from node 0:"
    0 dfs-iterative ;
```

## How it works:

1. **Graph Representation**: Uses adjacency lists to represent the graph
2. **Stack Management**: Implements stack operations for iterative DFS
3. **Visited Tracking**: Keeps track of visited nodes to avoid cycles
4. **Two Implementations**:
   - Recursive DFS (simpler but limited by stack depth)
   - Iterative DFS (more robust, uses explicit stack)

## Key Features:

- **Memory Efficient**: Uses arrays for storage
- **Flexible**: Can handle any number of nodes (up to MAX_NODES)
- **Cycle Detection**: Prevents infinite loops in cyclic graphs
- **Print Output**: Shows the traversal order for demonstration

## Usage:
```forth
test-dfs \ Runs the example DFS traversal
```

This implementation demonstrates the core concepts of DFS traversal while working within Forth's stack-based architecture and memory management model.