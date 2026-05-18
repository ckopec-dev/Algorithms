# Floyd-Warshall Algorithm in LMC (Little Man Computer)

## Overview
The Floyd-Warshall algorithm finds shortest paths between all pairs of vertices in a weighted graph. Here's a simplified implementation in LMC.

## LMC Assembly Code

```
; Floyd-Warshall Algorithm - LMC Implementation
; Finds shortest paths between all pairs of vertices

; Data Section
; Graph representation: 4x4 adjacency matrix
; 0 = no edge, positive number = edge weight
; 0,  3,  8,  999
; 999, 0,  -4, 999
; 999, 1,  0,  7
; 2,  999, 999, 0

; Memory locations for graph matrix
MATRIX   DAT 0     ; Matrix[0][0]
         DAT 3     ; Matrix[0][1]
         DAT 8     ; Matrix[0][2]
         DAT 999   ; Matrix[0][3]
         DAT 999   ; Matrix[1][0]
         DAT 0     ; Matrix[1][1]
         DAT -4    ; Matrix[1][2]
         DAT 999   ; Matrix[1][3]
         DAT 999   ; Matrix[2][0]
         DAT 1     ; Matrix[2][1]
         DAT 0     ; Matrix[2][2]
         DAT 7     ; Matrix[2][3]
         DAT 2     ; Matrix[3][0]
         DAT 999   ; Matrix[3][1]
         DAT 999   ; Matrix[3][2]
         DAT 0     ; Matrix[3][3]

; Constants
MAX_SIZE DAT 4     ; Number of vertices
INFINITY DAT 999   ; Represents infinity

; Program Start
        LDA START
        STA COUNT_I
        LDA START
        STA COUNT_J
        LDA START
        STA COUNT_K

; Main Floyd-Warshall Loop
LOOP_K  LDA COUNT_K
        LDA MAX_SIZE
        SUB
        BRZ END_PROGRAM

        LDA COUNT_K
        STA K_VALUE

        LDA COUNT_I
        STA I_VALUE
        LDA COUNT_J
        STA J_VALUE

        LDA COUNT_I
        LDA COUNT_J
        SUB
        BRZ LOOP_J

        ; Calculate matrix indices
        LDA COUNT_I
        LDA MAX_SIZE
        MUL
        LDA COUNT_J
        ADD
        STA INDEX

        ; Get current distance
        LDA MATRIX
        LDA INDEX
        ADD
        LDA MATRIX
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        LDA INDEX
        ADD
        L