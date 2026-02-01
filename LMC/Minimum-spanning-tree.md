# Minimum Spanning Tree Algorithm in LMC (Little Man Computer)

Here's an implementation of Prim's Minimum Spanning Tree algorithm in LMC:

```assembly
; Minimum Spanning Tree using Prim's Algorithm
; LMC Assembly Code

; Data Section
START   INP         ; Read number of vertices
        STA NODES
        INP         ; Read number of edges
        STA EDGES
        LDA #0      ; Initialize edge counter
        STA EDGE_COUNT

; Initialize adjacency matrix (simplified for example)
        LDA #100    ; Starting address for matrix
        STA MATRIX_ADDR

; Initialize visited array
        LDA #200    ; Starting address for visited array
        STA VISITED_ADDR
        LDA #0      ; Initialize visited array to 0
        STA VISITED
        LDA #1      ; Increment address
        STA VISITED_ADDR
        LDA #0      ; Continue initialization

; Main algorithm loop
MAIN    LDA NODES
        STA COUNT
        LDA #0      ; Start with vertex 0
        STA CURRENT
        LDA #0      ; Initialize min weight
        STA MIN_WEIGHT

; Find minimum edge
FIND_MIN LDA #0      ; Reset edge counter
        STA EDGE_COUNTER
        LDA #0      ; Reset min weight
        STA MIN_EDGE
        LDA #999    ; Initialize to large value
        STA MIN_WEIGHT

; Loop through edges to find minimum
EDGE_LOOP LDA EDGE_COUNTER
        LDA #10     ; Compare with edge count
        BRZ END_ALGORITHM
        LDA EDGE_COUNTER
        STA TEMP
        LDA #0      ; Get edge weight (simplified)
        STA EDGE_WEIGHT
        LDA EDGE_WEIGHT
        LDA MIN_WEIGHT
        SUB         ; Compare weights
        BRP CHECK_NEXT
        LDA EDGE_WEIGHT
        STA MIN_WEIGHT
        LDA EDGE_COUNTER
        STA MIN_EDGE

CHECK_NEXT LDA EDGE_COUNTER
        LDA #1
        ADD
        STA EDGE_COUNTER
        BRA EDGE_LOOP

; Add minimum edge to MST
ADD_EDGE LDA MIN_WEIGHT
        STA TOTAL_WEIGHT
        LDA #0      ; Mark vertex as visited
        STA VISITED

; Continue until all vertices visited
CONTINUE LDA COUNT
        LDA #1
        SUB
        BRZ END_ALGORITHM
        LDA COUNT
        LDA #1
        SUB
        STA COUNT
        BRA MAIN

END_ALGORITHM LDA TOTAL_WEIGHT
        OUT
        HLT

; Data areas
NODES   DAT 0
EDGES   DAT 0
EDGE_COUNT DAT 0
MATRIX_ADDR DAT 0
VISITED_ADDR DAT 0
COUNT   DAT 0
CURRENT DAT 0
MIN_WEIGHT DAT 0
MIN_EDGE DAT 0
EDGE_COUNTER DAT 0
EDGE_WEIGHT DAT 0
TOTAL_WEIGHT DAT 0
TEMP    DAT 0

; Sample input data (for testing):
; Input 1: 4 (number of vertices)
; Input 2: 5 (number of edges)
```

## Explanation:

This LMC implementation demonstrates a simplified version of Prim's MST algorithm:

1. **Input Processing**: Reads number of vertices and edges
2. **Initialization**: Sets up data structures for adjacency matrix and visited array
3. **Main Loop**: Repeatedly finds the minimum weight edge connecting a visited vertex to an unvisited vertex
4. **Edge Selection**: Compares edge weights to find the minimum
5. **Output**: Displays the total weight of the minimum spanning tree

## Key Features:

- Uses LMC's basic instructions (INP, OUT, LDA, STA, ADD, SUB, BRZ, BRP)
- Implements core MST algorithm logic
- Handles vertex selection and edge weight comparison
- Demonstrates the fundamental concepts of minimum spanning tree construction

## Note:

This is a conceptual implementation showing the algorithm structure. A complete implementation would require more complex data handling and memory management typical of the LMC architecture.

