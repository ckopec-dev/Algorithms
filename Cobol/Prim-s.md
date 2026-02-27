# Prim's Algorithm in COBOL

Here's an example implementation of Prim's algorithm for finding the Minimum Spanning Tree (MST) in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRIMS-ALGORITHM.
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
          05 NODE-A        PIC 9(3).
          05 NODE-B        PIC 9(3).
          05 WEIGHT        PIC 9(3).

       WORKING-STORAGE SECTION.
       01 GRAPH-SIZE        PIC 9(3) VALUE 6.
       01 INF               PIC 9(5) VALUE 99999.
       01 I                 PIC 9(3).
       01 J                 PIC 9(3).
       01 K                 PIC 9(3).
       01 MIN-WEIGHT        PIC 9(5).
       01 MIN-INDEX         PIC 9(3).
       01 TOTAL-WEIGHT      PIC 9(5) VALUE 0.
       01 EDGE-COUNT        PIC 9(3) VALUE 0.

       01 GRAPH-TABLE.
          05 NODE-TABLE OCCURS 10 TIMES.
             10 NODE-WEIGHT   PIC 9(5) VALUE 0.
             10 NODE-SELECTED PIC 9 VALUE 0.
             10 NODE-PARENT   PIC 9(3) VALUE 0.

       01 ADJACENCY-MATRIX.
          05 MATRIX OCCURS 10 TIMES INDEXED BY I-J.
             10 MATRIX-ROW OCCURS 10 TIMES.
                15 EDGE-WEIGHT PIC 9(5) VALUE 0.

       01 FILE-STATUS       PIC XX VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-GRAPH.
           PERFORM PRIMS-ALGORITHM.
           PERFORM DISPLAY-RESULTS.
           STOP RUN.

       INITIALIZE-GRAPH.
           PERFORM INITIALIZE-MATRIX.
           PERFORM READ-INPUT-DATA.
           PERFORM DISPLAY-INITIAL-MATRIX.

       INITIALIZE-MATRIX.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRAPH-SIZE
                   MOVE INF TO EDGE-WEIGHT(I-J)
               END-PERFORM
               MOVE 0 TO NODE-WEIGHT(I)
               MOVE 0 TO NODE-SELECTED(I)
               MOVE 0 TO NODE-PARENT(I)
           END-PERFORM.

       READ-INPUT-DATA.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE
               AT END GO TO END-OF-FILE
               NOT AT END PERFORM PROCESS-RECORD
           END-READ.

       PROCESS-RECORD.
           MOVE NODE-A TO I.
           MOVE NODE-B TO J.
           MOVE WEIGHT TO EDGE-WEIGHT(I-J).
           MOVE WEIGHT TO EDGE-WEIGHT(J-I).
           READ INPUT-FILE
               AT END GO TO END-OF-FILE
               NOT AT END PERFORM PROCESS-RECORD
           END-READ.

       END-OF-FILE.
           CLOSE INPUT-FILE.

       PRIMS-ALGORITHM.
           MOVE 1 TO MIN-INDEX.
           MOVE 1 TO NODE-SELECTED(MIN-INDEX).
           MOVE 0 TO NODE-PARENT(MIN-INDEX).
           MOVE 0 TO EDGE-COUNT.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE - 1
               PERFORM FIND-MINIMUM-EDGE.
               PERFORM ADD-TO-MST.
           END-PERFORM.

       FIND-MINIMUM-EDGE.
           MOVE INF TO MIN-WEIGHT.
           MOVE 0 TO MIN-INDEX.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
               IF NODE-SELECTED(I) = 1
                   PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRAPH-SIZE
                       IF NODE-SELECTED(J) = 0
                           IF EDGE-WEIGHT(I-J) < MIN-WEIGHT
                               MOVE EDGE-WEIGHT(I-J) TO MIN-WEIGHT
                               MOVE J TO MIN-INDEX
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM.

       ADD-TO-MST.
           MOVE 1 TO NODE-SELECTED(MIN-INDEX).
           MOVE NODE-PARENT(MIN-INDEX) TO NODE-PARENT(MIN-INDEX).
           ADD MIN-WEIGHT TO TOTAL-WEIGHT.
           ADD 1 TO EDGE-COUNT.

       DISPLAY-RESULTS.
           DISPLAY "MINIMUM SPANNING TREE RESULTS".
           DISPLAY "===========================".
           DISPLAY "Total Weight: " TOTAL-WEIGHT.
           DISPLAY "Number of Edges: " EDGE-COUNT.
           DISPLAY "Edge   From   To   Weight".
           DISPLAY "----   ----   --   ------".
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
               IF NODE-PARENT(I) > 0
                   DISPLAY I "      " NODE-PARENT(I) "     " NODE-WEIGHT(I)
               END-IF
           END-PERFORM.

       DISPLAY-INITIAL-MATRIX.
           DISPLAY "ADJACENCY MATRIX".
           DISPLAY "=================".
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRAPH-SIZE
                   DISPLAY "Node " I " to " J " : " EDGE-WEIGHT(I-J)
               END-PERFORM
               DISPLAY "---------------------"
           END-PERFORM.
```

## Sample Input File (GRAPH.DAT)
```
1 2 10
1 3 6
1 4 5
2 4 15
3 4 4
4 5 8
4 6 9
5 6 11
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses adjacency matrix to store graph edges
2. **Prim's Algorithm Logic**: Implements the greedy approach to find MST
3. **Data Structures**: 
   - Adjacency matrix for edge weights
   - Node selection tracking
   - Parent tracking for MST edges
4. **File Handling**: Reads graph data from external file
5. **Output**: Displays the minimum spanning tree with total weight

## Algorithm Steps:
1. Initialize all nodes as unvisited
2. Start with any node (node 1 in this case)
3. Find the minimum weight edge connecting visited and unvisited nodes
4. Add that edge to MST
5. Repeat until all nodes are included

This implementation demonstrates the classic Prim's algorithm in COBOL with proper file handling and data structure management.

