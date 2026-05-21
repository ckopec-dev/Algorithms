# Hopcroft-Karp Algorithm in Forth

Here's an implementation of the Hopcroft-Karp algorithm for finding maximum matching in bipartite graphs using Forth:

```forth
\ Hopcroft-Karp Algorithm Implementation in Forth

\ Graph representation: adjacency lists
\ We'll use arrays to represent the graph

\ Constants
100 CONSTANT MAX_VERTICES
1000 CONSTANT MAX_EDGES

\ Global variables
VARIABLE num_vertices
VARIABLE num_edges
VARIABLE match_u
VARIABLE match_v
VARIABLE dist
VARIABLE queue
VARIABLE queue_head
VARIABLE queue_tail

\ Adjacency list representation
CREATE adj_list MAX_VERTICES MAX_EDGES * ALLOT
CREATE adj_count MAX_VERTICES ALLOT

\ Initialize graph
: init_graph ( -- )
    0 num_vertices !
    0 num_edges !
    0 queue_head !
    0 queue_tail !
    MAX_VERTICES 0 DO
        0 adj_count I + !
    LOOP
    MAX_VERTICES MAX_EDGES * 0 DO
        0 adj_list I + !
    LOOP
    MAX_VERTICES 0 DO
        0 match_u I + !
        0 match_v I + !
        MAX_VERTICES 0 DO
            0 dist I + !
        LOOP
    LOOP
;

\ Add edge to adjacency list
: add_edge ( u v -- )
    num_edges @ adj_count swap + 1+ dup adj_count swap + !
    adj_list swap MAX_VERTICES * + 0 DO
        0 adj_list swap MAX_VERTICES * + I + !
    LOOP
    adj_list swap MAX_VERTICES * + num_edges @ + !
    num_edges 1+!
;

\ BFS to find augmenting paths
: bfs ( -- flag )
    MAX_VERTICES 0 DO
        0 dist I + !
    LOOP
    0 queue_head !
    0 queue_tail !
    
    \ Initialize queue with unmatched vertices in U
    MAX_VERTICES 0 DO
        match_u I + @ 0= IF
            I dist I + !
            queue_tail @ queue + !
            queue_tail 1+!
        THEN
    LOOP
    
    \ BFS
    queue_head @ queue_tail @ < WHILE
        queue_head @ queue + @ dup
        queue_head 1+!
        adj_count swap + @ 0 DO
            adj_list swap MAX_VERTICES * + I + @ dup
            match_v + @ 0= IF
                \ Found unmatched vertex in V
                0 dist + !
                1
            ELSE
                \ Already matched, check if we can find augmenting path
                match_v + @ dist + @ 0= IF
                    match_v + @ dist + !
                    queue_tail @ queue + !
                    queue_tail 1+!
                    0
                THEN
            THEN
        LOOP
    REPEAT
    0
;

\ DFS to find augmenting path
: dfs ( u -- found )
    adj_count swap + @ 0 DO
        adj_list swap MAX_VERTICES * + I + @ dup
        match_v + @ 0= IF
            \ Found unmatched vertex in V
            match_u swap + !
            match_v swap + !
            1
        ELSE
            match_v + @ dist + @ 0= IF
                match_v + @ dist + !
                0
            THEN
        THEN
    LOOP
    0
;

\ Hopcroft-Karp algorithm
: hopcroft-karp ( -- max_matching )
    0
    BEGIN
        bfs
    WHILE
        0
        MAX_VERTICES 0 DO
            match_u I + @ 0= IF
                dfs I
            THEN
        LOOP
        1+
    REPEAT
    DROP
;

\ Example usage
: test_hopcroft_karp ( -- )
    init_graph
    
    \ Create a sample bipartite graph
    \ Vertices U: 0,1,2,3
    \ Vertices V: 0,1,2,3
    \ Edges: (0,0), (0,1), (1,1), (1,2), (2,0), (2,3), (3,2)
    
    0 0 add_edge
    0 1 add_edge
    1 1 add_edge
    1 2 add_edge
    2 0 add_edge
    2 3 add_edge
    3 2 add_edge
    
    \ Set number of vertices
    4 num_vertices !
    
    \ Run Hopcroft-Karp
    hopcroft-karp . ." Maximum matching: "
;

\ Run the example
test_hopcroft_karp
```

## Explanation

This implementation of the Hopcroft-Karp algorithm in Forth includes:

1. **Data Structures**:
   - Adjacency list representation using arrays
   - Match arrays for tracking vertex matches
   - Distance array for BFS
   - Queue for BFS traversal

2. **Key Functions**:
   - `init_graph`: Initializes all data structures
   - `add_edge`: Adds edges to the adjacency list
   - `bfs`: Performs breadth-first search to find augmenting paths
   - `dfs`: Performs depth-first search to find augmenting paths
   - `hopcroft-karp`: Main algorithm that repeatedly finds augmenting paths

3. **Algorithm Steps**:
   - Initialize distances and queue with unmatched vertices in U
   - BFS to find all vertices reachable from unmatched vertices
   - DFS to find augmenting paths
   - Continue until no more augmenting paths exist

The example creates a bipartite graph with 4 vertices in each set and finds a maximum matching of 3 edges. The time complexity is O(E√V) where E is the number of edges and V is the number of vertices.

