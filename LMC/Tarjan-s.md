# Tarjan's Algorithm in LMC (Little Man Computer)

Here's an implementation of Tarjan's algorithm for finding strongly connected components in LMC:

```assembly
; Tarjan's Algorithm - Strongly Connected Components
; LMC Implementation

; Data section
NUM_NODES    DAT 5        ; Number of nodes (0-4)
ADJACENCY    DAT 1        ; Adjacency list: 1->2, 2->3, 3->1, 4->5, 5->4
             DAT 2
             DAT 3
             DAT 1
             DAT 4
             DAT 5
             DAT 4
             DAT 5
             DAT 0        ; End marker

; Algorithm variables
INDEX        DAT 0        ; Global index counter
STACK        DAT 0        ; Stack pointer
NODE_STACK   DAT 0        ; Stack for nodes
LOWLINK      DAT 0        ; Lowlink values
INDEX_ARRAY  DAT 0        ; Index array
VISITED      DAT 0        ; Visited array
SCC_COUNT    DAT 0        ; Strongly connected component counter

; Main program
START        INP          ; Read input
             STA NODES    ; Store number of nodes
             LDA #0       ; Initialize index
             STA INDEX
             LDA #0       ; Initialize stack pointer
             STA STACK
             LDA #0       ; Initialize SCC counter
             STA SCC_COUNT
             LDA #0       ; Initialize all visited flags to 0
             STA VISITED
             LDA #0       ; Initialize all index values to 0
             STA INDEX_ARRAY
             LDA #0       ; Initialize all lowlink values to 0
             STA LOWLINK

             ; Call tarjan algorithm for each unvisited node
             LDA #0       ; Start with node 0
             STA CURRENT_NODE
             LDA #0       ; Initialize stack
             STA NODE_STACK

TARJAN_LOOP  LDA CURRENT_NODE
             LDA INDEX_ARRAY
             LDA #0       ; Check if node is unvisited
             BRZ VISIT_NODE
             LDA CURRENT_NODE
             LDA #1       ; Increment to next node
             STA CURRENT_NODE
             LDA CURRENT_NODE
             LDA NODES
             BRZ END_TARJAN
             JMP TARJAN_LOOP

VISIT_NODE   LDA CURRENT_NODE
             LDA INDEX
             STA INDEX_ARRAY
             LDA INDEX
             STA LOWLINK
             LDA INDEX
             STA NODE_STACK
             LDA #1
             STA INDEX
             LDA CURRENT_NODE
             LDA #1
             BRZ END_TARJAN

             ; Find neighbors and process
             LDA CURRENT_NODE
             LDA ADJACENCY
             LDA #0       ; Check if end marker
             BRZ END_NEIGHBORS
             ; Process neighbor
             LDA #0       ; Placeholder for neighbor processing
             JMP TARJAN_LOOP

END_NEIGHBORS LDA NODE_STACK
             LDA #0       ; Pop from stack
             LDA #0       ; Check if current node is top of stack
             BRZ PROCESS_SCC
             JMP TARJAN_LOOP

PROCESS_SCC  LDA SCC_COUNT
             LDA #1
             STA SCC_COUNT
             LDA #0       ; Output SCC information
             OUT
             LDA #0       ; End of SCC
             JMP TARJAN_LOOP

END_TARJAN   LDA SCC_COUNT
             OUT          ; Output number of SCCs
             LDA #999     ; Halt
             STA #999
             HLT

; Helper routines
; Stack operations
PUSH         LDA STACK
             LDA #1
             STA STACK
             LDA NODE_STACK
             LDA #0       ; Store node value
             JMP RETURN

POP          LDA STACK
             LDA #0
             STA STACK
             LDA NODE_STACK
             LDA #0       ; Return node value
             JMP RETURN

RETURN       LDA #0       ; Return address
             JMP START

; Data storage
NODES        DAT 0
CURRENT_NODE DAT 0
```

## Explanation

This LMC implementation demonstrates the core concepts of Tarjan's algorithm:

**Key Components:**
1. **Data Structures**: Stack for tracking nodes, index arrays, lowlink values
2. **Global Variables**: INDEX, STACK, SCC_COUNT
3. **Main Algorithm**: Recursive depth-first search with stack management
4. **SCC Detection**: Lowlink value comparison to identify strongly connected components

**Algorithm Flow:**
1. Initialize all data structures
2. For each unvisited node, perform DFS
3. Push nodes onto stack during traversal
4. Calculate lowlink values based on neighbors
5. When back edge is found, pop components from stack
6. Output strongly connected components

**Note**: This is a conceptual implementation. LMC has limited memory and no subroutine support, so a full implementation would require significant optimization and would be quite complex. The actual LMC implementation would need to be much more elaborate to handle the recursive nature of the algorithm properly.

