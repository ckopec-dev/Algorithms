# Bellman-Ford Algorithm in LMC (Little Man Computer)

Here's an implementation of the Bellman-Ford algorithm in LMC assembly language. This example finds the shortest paths from a source vertex to all other vertices in a weighted graph.

```assembly
; Bellman-Ford Algorithm in LMC
; Finds shortest paths from vertex 0 to all other vertices
; Graph with 4 vertices (0,1,2,3) and 6 edges

; Data section
START   INP         ; Read number of vertices (4)
        STA VERTEX
        INP         ; Read number of edges (6)
        STA EDGE
        LDA ZERO
        STA COUNT   ; Initialize counter to 0

; Initialize distance array with infinity (999) except source (0)
        LDA ZERO
        STA DIST0   ; Distance to vertex 0 = 0
        LDA NINE99
        STA DIST1   ; Distance to vertex 1 = 999
        STA DIST2   ; Distance to vertex 2 = 999
        STA DIST3   ; Distance to vertex 3 = 999

; Read edges (source, destination, weight)
        LDA ZERO
        STA EDGE_COUNT

READ_EDGE LDA EDGE_COUNT
        BRZ READ_END
        INP         ; Read source vertex
        STA SOURCE
        INP         ; Read destination vertex
        STA DEST
        INP         ; Read edge weight
        STA WEIGHT

        ; Store edge data in memory locations
        LDA SOURCE
        STA EDGE_SRC
        LDA DEST
        STA EDGE_DEST
        LDA WEIGHT
        STA EDGE_WGT

        LDA EDGE_COUNT
        ADD ONE
        STA EDGE_COUNT
        BRA READ_EDGE

READ_END LDA ZERO
        STA ITER_COUNT

; Main Bellman-Ford loop - relax edges V-1 times
RELAX_LOOP LDA ITER_COUNT
        BRZ DONE
        LDA VERTEX
        SUB ONE
        STA ITER_COUNT

        ; For each edge, relax it
        LDA ZERO
        STA EDGE_INDEX

RELAX_EDGE LDA EDGE_INDEX
        BRZ RELAX_END
        LDA EDGE_INDEX
        ADD ONE
        STA EDGE_INDEX

        ; Get source, destination, weight from edge data
        LDA EDGE_SRC
        STA TEMP1
        LDA EDGE_DEST
        STA TEMP2
        LDA EDGE_WGT
        STA TEMP3

        ; Relax edge: if dist[u] + weight < dist[v], update dist[v]
        LDA DIST0
        BRZ CHECK1
        LDA DIST1
        BRZ CHECK2
        LDA DIST2
        BRZ CHECK3
        LDA DIST3
        BRZ CHECK4

CHECK1  LDA DIST0
        ADD TEMP3
        STA TEMP4
        LDA DIST1
        BRZ UPDATE1
        SUB TEMP4
        BRZ UPDATE1
        BRZ SKIP1

UPDATE1 LDA TEMP4
        STA DIST1
        BRA RELAX_EDGE

CHECK2  LDA DIST1
        ADD TEMP3
        STA TEMP4
        LDA DIST2
        BRZ UPDATE2
        SUB TEMP4
        BRZ UPDATE2
        BRZ SKIP2

UPDATE2 LDA TEMP4
        STA DIST2
        BRA RELAX_EDGE

CHECK3  LDA DIST2
        ADD TEMP3
        STA TEMP4
        LDA DIST3
        BRZ UPDATE3
        SUB TEMP4
        BRZ UPDATE3
        BRZ SKIP3

UPDATE3 LDA TEMP4
        STA DIST3
        BRA RELAX_EDGE

CHECK4  LDA DIST3
        ADD TEMP3
        STA TEMP4
        LDA DIST0
        BRZ UPDATE4
        SUB TEMP4
        BRZ UPDATE4
        BRZ SKIP4

UPDATE4 LDA TEMP4
        STA DIST0
        BRA RELAX_EDGE

SKIP1   BRA RELAX_EDGE
SKIP2   BRA RELAX_EDGE
SKIP3   BRA RELAX_EDGE
SKIP4   BRA RELAX_EDGE

RELAX_END LDA ZERO
        STA EDGE_INDEX
        BRA RELAX_LOOP

DONE    ; Output results
        LDA DIST0
        OUT
        LDA DIST1
        OUT
        LDA DIST2
        OUT
        LDA DIST3
        OUT

        LDA HLT
        STA HLT_ADDR

; Data definitions
VERTEX  DAT
EDGE    DAT
COUNT   DAT
DIST0   DAT 0
DIST1   DAT 999
DIST2   DAT 999
DIST3   DAT 999
SOURCE  DAT
DEST    DAT
WEIGHT  DAT
EDGE_COUNT DAT
EDGE_INDEX DAT
TEMP1   DAT
TEMP2   DAT
TEMP3   DAT
TEMP4   DAT
ITER_COUNT DAT

; Constants
ZERO    DAT 0
ONE     DAT 1
NINE99  DAT 999
HLT     DAT 0
HLT_ADDR DAT 0
EDGE_SRC DAT
EDGE_DEST DAT
EDGE_WGT DAT
```

## Key Features of this LMC Implementation:

1. **Graph Representation**: Uses a simple edge list format with source, destination, and weight
2. **Distance Array**: Maintains distances to each vertex (0 for source, 999 for infinity)
3. **Main Loop**: Executes V-1 iterations (where V = number of vertices)
4. **Edge Relaxation**: For each edge, checks if a shorter path exists
5. **Output**: Displays final shortest distances from source vertex

## How it works:

1. **Initialization**: Sets up the distance array with source vertex distance = 0, others = 999
2. **Edge Reading**: Reads all edges and stores them in memory
3. **Relaxation**: Repeatedly relaxes all edges for V-1 iterations
4. **Result**: Outputs the shortest distances to all vertices

## Note:
This is a simplified version that demonstrates the core concepts. A complete implementation would need to handle:
- More sophisticated memory management
- Proper edge list storage
- Error handling
- Loop termination conditions
- More complex graph structures

The LMC version is constrained by its limited instruction set and memory, making it a challenging but educational exercise in algorithm implementation.

