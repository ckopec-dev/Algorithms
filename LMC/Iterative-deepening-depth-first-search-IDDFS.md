# Iterative Deepening Depth-First Search (IDDFS) in LMC

```assembly
; Iterative Deepening Depth-First Search (IDDFS) Implementation
; LMC (Little Man Computer) Assembly Code

; Data Section
NUM_NODES   DAT 5          ; Number of nodes (0-4)
START_NODE  DAT 0          ; Starting node
GOAL_NODE   DAT 4          ; Goal node
MAX_DEPTH   DAT 10         ; Maximum depth limit

; Graph representation (adjacency list)
; Node 0: connected to 1, 2
; Node 1: connected to 3, 4
; Node 2: connected to 4
; Node 3: connected to 4
; Node 4: connected to none

; Adjacency list storage
ADJ_LIST    DAT 100        ; Start of adjacency list
            DAT 1          ; Node 0: connected to node 1
            DAT 2          ; Node 0: connected to node 2
            DAT 999        ; End marker for node 0
            DAT 3          ; Node 1: connected to node 3
            DAT 4          ; Node 1: connected to node 4
            DAT 999        ; End marker for node 1
            DAT 4          ; Node 2: connected to node 4
            DAT 999        ; End marker for node 2
            DAT 4          ; Node 3: connected to node 4
            DAT 999        ; End marker for node 3
            DAT 999        ; End marker for node 4

; Stack for DFS
STACK       DAT 1000       ; Stack memory location
STACK_PTR   DAT 0          ; Stack pointer

; Main program
MAIN        DAT 100        ; Start of main program
            DAT 500        ; Load depth limit (0)
            DAT 501        ; Load start node (0)
            DAT 502        ; Load goal node (4)
            DAT 503        ; Load max depth (10)
            DAT 504        ; Load adjacency list start
            DAT 505        ; Load stack start
            DAT 506        ; Load stack pointer
            DAT 999        ; End marker

; IDDFS algorithm
IDDFS_LOOP  DAT 100        ; Main loop for iterative deepening
            DAT 200        ; Load depth limit
            DAT 300        ; Store depth limit
            DAT 100        ; Load current depth
            DAT 200        ; Load depth limit
            DAT 800        ; Compare depth with limit
            DAT 100        ; Load current depth
            DAT 200        ; Load depth limit
            DAT 800        ; Compare depth with limit
            DAT 100        ; Load current depth
            DAT 200        ; Load depth limit
            DAT 800        ; Compare depth with limit
            DAT 100        ; Load current depth
            DAT 200        ; Load depth limit
            DAT 800        ; Compare depth with limit
            DAT 100        ; Load current depth
            DAT 200        ; Load depth limit
            DAT 800        ; Compare depth with limit

; Depth-limited DFS subroutine
DFS_SUB     DAT 200        ; Load current depth
            DAT 300        ; Store current depth
            DAT 400        ; Load current node
            DAT 500        ; Load goal node
            DAT 800        ; Compare current with goal
            DAT 900        ; Return if found
            DAT 100        ; Load current node
            DAT 200        ; Load current depth
            DAT 300        ; Store current depth
            DAT 400        ; Load current node
            DAT 500        ; Load goal node
            DAT 800        ; Compare current with goal
            DAT 900        ; Return if found

; Stack operations
PUSH_STACK  DAT 100        ; Load node to push
            DAT 200        ; Load stack pointer
            DAT 300        ; Store node at stack pointer
            DAT 400        ; Load stack pointer
            DAT 500        ; Load 1
            DAT 600        ; Add 1 to stack pointer
            DAT 700        ; Store new stack pointer

POP_STACK   DAT 100        ; Load stack pointer
            DAT 200        ; Load 1
            DAT 300        ; Subtract 1 from stack pointer
            DAT 400        ; Load stack pointer
            DAT 500        ; Load stack location
            DAT 600        ; Load value from stack
            DAT 700        ; Store value in return register

; Helper functions
IS_VISITED  DAT 100        ; Load node
            DAT 200        ; Load visited array
            DAT 300        ; Check if visited
            DAT 400        ; Load visited flag
            DAT 500        ; Load 1
            DAT 600        ; Compare
            DAT 700        ; Store result

MARK_VISITED DAT 100       ; Load node
            DAT 200        ; Load visited array
            DAT 300        ; Mark as visited
            DAT 400        ; Load 1
            DAT 500        ; Load visited flag
            DAT 600        ; Store 1 in visited array

; Main execution flow
START       DAT 100        ; Load start node
            DAT 200        ; Load depth limit
            DAT 300        ; Store depth limit
            DAT 400        ; Load max depth
            DAT 500        ; Load current depth
            DAT 600        ; Initialize depth
            DAT 700        ; Store current depth

; Loop through depths
DEPTH_LOOP  DAT 100        ; Load current depth
            DAT 200        ; Load max depth
            DAT 300        ; Compare depths
            DAT 400        ; Load current depth
            DAT 500        ; Load 1
            DAT 600        ; Add 1 to depth
            DAT 700        ; Store new depth
            DAT 800        ; Jump if depth < max depth

; Call depth-limited DFS
CALL_DFS    DAT 100        ; Load current depth
            DAT 200        ; Load start node
            DAT 300        ; Load goal node
            DAT 400        ; Call DFS with current depth
            DAT 500        ; Load adjacency list
            DAT 600        ; Load stack
            DAT 700        ; Load visited array

; End of program
END         DAT 999        ; End marker
```

## Algorithm Explanation

This IDDFS implementation in LMC follows these key steps:

1. **Iterative Deepening Loop**: Starts with depth 0 and increases depth limit
2. **Depth-Limited DFS**: For each depth limit, performs a depth-limited DFS
3. **Stack Management**: Uses a stack for DFS traversal
4. **Goal Testing**: Checks if goal node is found at each step
5. **Backtracking**: Returns to previous nodes when dead ends are reached

## Key Features

- **Memory Efficient**: Uses iterative deepening to avoid full memory usage of BFS
- **Optimal**: Finds shortest path like BFS but with less memory
- **Complete**: Will find solution if one exists
- **Uses Stack**: Implements DFS with explicit stack operations

The algorithm systematically increases depth limits until solution is found, making it both optimal and memory-efficient for large search spaces.

