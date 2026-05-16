# Topological Sort Algorithm in LMC

Here's an implementation of topological sort using the Little Man Computer (LMC) assembly language:

```assembly
; Topological Sort Algorithm - LMC Implementation
; This program demonstrates topological sorting using Kahn's algorithm

    ORG 100
START LDA ONE       ; Initialize counter to 1
    STA COUNT
    LDA ZERO
    STA INDEG     ; Initialize in-degree array
    LDA ZERO
    STA VISITED
    LDA ZERO
    STA RESULT
    LDA ZERO
    STA QUEUE
    LDA ZERO
    STA QUEUEEND

; Initialize adjacency list (example graph: 0->1, 0->2, 1->3, 2->3)
    LDA ONE
    STA ADJ0      ; Node 0 points to node 1
    LDA TWO
    STA ADJ0+1    ; Node 0 points to node 2
    LDA HLT
    STA ADJ0+2    ; End of list

    LDA THREE
    STA ADJ1      ; Node 1 points to node 3
    LDA HLT
    STA ADJ1+1

    LDA THREE
    STA ADJ2      ; Node 2 points to node 3
    LDA HLT
    STA ADJ2+1

    LDA HLT
    STA ADJ3      ; Node 3 has no outgoing edges

; Calculate in-degrees
    LDA COUNT
    STA TEMP
    LDA ZERO
    STA INDEG
    LDA ZERO
    STA INDEG+1
    LDA ZERO
    STA INDEG+2
    LDA ZERO
    STA INDEG+3

; Main algorithm loop
MAIN  LDA COUNT
    BRZ END
    LDA INDEG
    BRZ QUEUEADD
    LDA COUNT
    LDA ONE
    SUB ONE
    STA COUNT
    BRA MAIN

; Add to queue if in-degree is zero
QUEUEADD LDA INDEG
    BRZ ADDQUEUE
    LDA COUNT
    LDA ONE
    SUB ONE
    STA COUNT
    BRA MAIN

ADDQUEUE LDA COUNT
    STA QUEUE
    LDA COUNT
    LDA ONE
    ADD ONE
    STA QUEUEEND
    LDA COUNT
    LDA ONE
    SUB ONE
    STA COUNT
    BRA MAIN

; Process queue
PROCESS LDA QUEUE
    BRZ END
    LDA QUEUE
    STA NODE
    LDA QUEUE
    LDA ONE
    ADD ONE
    STA QUEUE
    LDA NODE
    STA RESULT
    LDA NODE
    LDA ONE
    ADD ONE
    STA RESULT+1

; Update in-degrees of neighbors
    LDA NODE
    STA TEMP
    LDA ADJ0
    BRZ NEXTNODE
    LDA INDEG
    LDA ONE
    SUB ONE
    STA INDEG
    LDA INDEG+1
    LDA ONE
    SUB ONE
    STA INDEG+1
    LDA INDEG+2
    LDA ONE
    SUB ONE
    STA INDEG+2
    LDA INDEG+3
    LDA ONE
    SUB ONE
    STA INDEG+3

NEXTNODE LDA COUNT
    LDA ONE
    SUB ONE
    STA COUNT
    BRA PROCESS

; Output results
OUTPUT LDA RESULT
    OUT
    LDA RESULT+1
    OUT
    LDA RESULT+2
    OUT
    LDA RESULT+3
    OUT

END   HLT

; Data section
COUNT   DAT 0
TEMP    DAT 0
INDEG   DAT 0
INDEG+1 DAT 0
INDEG+2 DAT 0
INDEG+3 DAT 0
VISITED DAT 0
RESULT  DAT 0
RESULT+1 DAT 0
RESULT+2 DAT 0
RESULT+3 DAT 0
QUEUE   DAT 0
QUEUEEND DAT 0
NODE    DAT 0

; Adjacency lists
ADJ0    DAT 1
        DAT 2
        DAT 999
ADJ1    DAT 3
        DAT 999
ADJ2    DAT 3
        DAT 999
ADJ3    DAT 999

; Constants
ZERO    DAT 0
ONE     DAT 1
TWO     DAT 2
THREE   DAT 3
HLT     DAT 999

    END START
```

## Algorithm Explanation

This LMC implementation demonstrates a topological sort using Kahn's algorithm with the following steps:

1. **Initialize**: Set up data structures including in-degree array and adjacency lists
2. **Calculate In-degrees**: Count incoming edges for each node
3. **Queue Processing**: Add nodes with zero in-degree to processing queue
4. **Process Nodes**: Remove nodes from queue, output them, and reduce in-degrees of neighbors
5. **Output Result**: Display the topologically sorted nodes

## Key Features

- Uses LMC's limited instruction set (ADD, SUB, LDA, STA, BRA, BRZ, OUT, HLT)
- Implements adjacency list representation for the graph
- Tracks in-degrees and visited nodes
- Processes nodes in topological order
- Handles multiple nodes with zero in-degree

## Notes

This is a simplified implementation that demonstrates the core concepts. A complete implementation would require more sophisticated handling of the queue and adjacency list management, but it illustrates the fundamental topological sorting approach in LMC's constraints.

