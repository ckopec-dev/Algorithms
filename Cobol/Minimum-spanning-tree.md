# Minimum Spanning Tree Algorithm in COBOL

Here's an example implementation of Prim's Minimum Spanning Tree algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MINIMUM-SPANNING-TREE.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "GRAPH.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
          05 NODE-A            PIC 9(3).
          05 NODE-B            PIC 9(3).
          05 EDGE-WEIGHT       PIC 9(3).

       WORKING-STORAGE SECTION.
       01 GRAPH-SIZE            PIC 9(3) VALUE 6.
       01 MAX-EDGES             PIC 9(3) VALUE 10.
       01 I, J, K, L            PIC 9(3).
       01 MIN-WEIGHT            PIC 9(3).
       01 MIN-INDEX             PIC 9(3).
       01 TOTAL-WEIGHT          PIC 9(5) VALUE 0.
       01 TEMP-WEIGHT           PIC 9(3).

       01 GRAPH-MATRIX.
          05 MATRIX-ROW OCCURS 10 TIMES.
             10 MATRIX-ELEMENT OCCURS 10 TIMES PIC 9(3).

       01 SELECTED-NODES.
          05 SELECTED OCCURS 10 TIMES PIC 9(1) VALUE 0.

       01 EDGE-LIST.
          05 EDGE-RECORD OCCURS 10 TIMES.
             10 EDGE-A        PIC 9(3).
             10 EDGE-B        PIC 9(3).
             10 EDGE-W        PIC 9(3).

       01 OUTPUT-FILE.
          05 OUTPUT-RECORD.
             10 O-EDGE-A        PIC 9(3).
             10 O-EDGE-B        PIC 9(3).
             10 O-EDGE-W        PIC 9(3).
             10 O-TOTAL-W       PIC 9(5).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-GRAPH.
           PERFORM PRIMS-ALGORITHM.
           PERFORM DISPLAY-RESULTS.
           STOP RUN.

       INITIALIZE-GRAPH.
           PERFORM INITIALIZE-MATRIX.
           PERFORM LOAD-GRAPH-DATA.
           PERFORM INITIALIZE-SELECTED.

       INITIALIZE-MATRIX.
           MOVE 0 TO MATRIX-ELEMENT (1,1) THRU MATRIX-ELEMENT (10,10).

       INITIALIZE-SELECTED.
           MOVE 0 TO SELECTED (1) THRU SELECTED (10).

       LOAD-GRAPH-DATA.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE INTO INPUT-RECORD
               AT END GO TO LOAD-END.
           MOVE NODE-A TO EDGE-A (1).
           MOVE NODE-B TO EDGE-B (1).
           MOVE EDGE-WEIGHT TO EDGE-W (1).
           MOVE 1 TO K.
           PERFORM LOAD-RECORDS UNTIL END-OF-FILE.
           CLOSE INPUT-FILE.

       LOAD-RECORDS.
           READ INPUT-FILE INTO INPUT-RECORD
               AT END GO TO LOAD-END.
           ADD 1 TO K.
           MOVE NODE-A TO EDGE-A (K).
           MOVE NODE-B TO EDGE-B (K).
           MOVE EDGE-WEIGHT TO EDGE-W (K).

       LOAD-END.
           MOVE K TO MAX-EDGES.

       PRIMS-ALGORITHM.
           PERFORM PRIM-INITIALIZE.
           PERFORM PRIM-ITERATE UNTIL K = GRAPH-SIZE.

       PRIM-INITIALIZE.
           MOVE 1 TO SELECTED (1).
           MOVE 0 TO K.
           MOVE 0 TO TOTAL-WEIGHT.

       PRIM-ITERATE.
           ADD 1 TO K.
           PERFORM FIND-MINIMUM-EDGE.
           PERFORM ADD-TO-MST.
           PERFORM UPDATE-MATRIX.

       FIND-MINIMUM-EDGE.
           MOVE 999 TO MIN-WEIGHT.
           MOVE 0 TO MIN-INDEX.
           PERFORM FIND-WEIGHTS.

       FIND-WEIGHTS.
           MOVE 1 TO I.
           PERFORM FIND-WEIGHT-LOOP UNTIL I > MAX-EDGES.
           GO TO FIND-WEIGHTS-END.

       FIND-WEIGHT-LOOP.
           IF SELECTED (EDGE-A (I)) = 1 AND SELECTED (EDGE-B (I)) = 0
              OR SELECTED (EDGE-B (I)) = 1 AND SELECTED (EDGE-A (I)) = 0
              THEN
                 IF EDGE-W (I) < MIN-WEIGHT
                    THEN
                       MOVE EDGE-W (I) TO MIN-WEIGHT
                       MOVE I TO MIN-INDEX
                 END-IF
           END-IF.
           ADD 1 TO I.
           IF I <= MAX-EDGES GO TO FIND-WEIGHT-LOOP.

       FIND-WEIGHTS-END.

       ADD-TO-MST.
           IF MIN-INDEX > 0
              THEN
                 IF SELECTED (EDGE-A (MIN-INDEX)) = 1
                    THEN
                       MOVE EDGE-B (MIN-INDEX) TO J
                    ELSE
                       MOVE EDGE-A (MIN-INDEX) TO J
                 END-IF
                 MOVE 1 TO SELECTED (J).
                 ADD MIN-WEIGHT TO TOTAL-WEIGHT.
           END-IF.

       UPDATE-MATRIX.
           MOVE 0 TO I.
           PERFORM UPDATE-LOOP UNTIL I > MAX-EDGES.
           GO TO UPDATE-END.

       UPDATE-LOOP.
           IF EDGE-A (I) = J OR EDGE-B (I) = J
              THEN
                 MOVE 0 TO EDGE-W (I)
           END-IF.
           ADD 1 TO I.
           IF I <= MAX-EDGES GO TO UPDATE-LOOP.

       UPDATE-END.

       DISPLAY-RESULTS.
           DISPLAY "MINIMUM SPANNING TREE RESULTS:".
           DISPLAY "Total Weight: " TOTAL-WEIGHT.
           DISPLAY "Edges in MST:".
           PERFORM DISPLAY-EDGES.

       DISPLAY-EDGES.
           MOVE 1 TO I.
           PERFORM DISPLAY-EDGE-LOOP UNTIL I > MAX-EDGES.
           GO TO DISPLAY-END.

       DISPLAY-EDGE-LOOP.
           IF EDGE-W (I) > 0 AND SELECTED (EDGE-A (I)) = 1
              OR SELECTED (EDGE-B (I)) = 1
              THEN
                 DISPLAY "Edge: " EDGE-A (I) " - " EDGE-B (I) " Weight: " EDGE-W (I)
           END-IF.
           ADD 1 TO I.
           IF I <= MAX-EDGES GO TO DISPLAY-EDGE-LOOP.

       DISPLAY-END.
           DISPLAY "MST Complete. Total Weight: " TOTAL-WEIGHT.
```

## Sample Input File (GRAPH.DAT)
```
001002005
002003003
003004006
004005008
005006002
001006001
002006004
003006007
004006009
005006003
```

## Key Features of this Implementation:

1. **Data Structures**: Uses arrays for adjacency matrix and selected nodes
2. **Prim's Algorithm**: Implements the greedy approach to find MST
3. **File I/O**: Reads graph data from external file
4. **Memory Management**: Uses working storage for algorithm variables
5. **Error Handling**: Basic end-of-file handling

## Algorithm Steps:
1. Initialize graph matrix and selected nodes array
2. Start with any node (node 1)
3. Find minimum weight edge connecting selected and unselected nodes
4. Add this edge to MST
5. Repeat until all nodes are included

This COBOL implementation demonstrates how classical algorithms can be adapted for mainframe environments while maintaining the core logic of the Minimum Spanning Tree algorithm.

