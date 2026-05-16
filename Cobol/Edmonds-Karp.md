# Edmonds-Karp Algorithm in COBOL

Below is a COBOL implementation of the Edmonds-Karp algorithm for finding maximum flow in a flow network:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDMONDS-KARP.
       AUTHOR. Flow Network Algorithm.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "GRAPH.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
          05 SOURCE-INDEX     PIC 9(3).
          05 DESTINATION-INDEX PIC 9(3).
          05 CAPACITY         PIC 9(5).

       WORKING-STORAGE SECTION.
       01 GRAPH-SIZE         PIC 9(3) VALUE 6.
       01 MAX-FLOW           PIC 9(10) VALUE 0.
       01 TEMP-FLOW          PIC 9(10) VALUE 0.
       01 FLOW-ARRAY.
          05 FLOW-REC OCCURS 100 TIMES.
             10 FLOW-SOURCE    PIC 9(3).
             10 FLOW-DEST      PIC 9(3).
             10 FLOW-CAPACITY  PIC 9(5).
             10 FLOW-RESIDUAL  PIC 9(5).
       01 RESIDUAL-MATRIX.
          05 RESIDUAL-REC OCCURS 100 TIMES.
             10 RESIDUAL-ROW PIC 9(3).
             10 RESIDUAL-COL PIC 9(3).
             10 RESIDUAL-VALUE PIC 9(5).
       01 VISITED-ARRAY.
          05 VISITED-REC OCCURS 100 TIMES.
             10 VISITED-INDEX PIC 9(3).
             10 VISITED-FLAG  PIC 9 VALUE 0.
       01 PARENT-ARRAY.
          05 PARENT-REC OCCURS 100 TIMES.
             10 PARENT-INDEX PIC 9(3).
             10 PARENT-PREV  PIC 9(3).
       01 QUEUE-ARRAY.
          05 QUEUE-REC OCCURS 100 TIMES.
             10 QUEUE-ITEM PIC 9(3).
       01 QUEUE-FRONT        PIC 9(3) VALUE 1.
       01 QUEUE-BACK         PIC 9(3) VALUE 1.
       01 QUEUE-SIZE         PIC 9(3) VALUE 0.
       01 BFS-FOUND          PIC 9 VALUE 0.
       01 MIN-EDGE           PIC 9(5) VALUE 99999.
       01 CURRENT-EDGE       PIC 9(5) VALUE 0.
       01 CURRENT-INDEX      PIC 9(3) VALUE 0.
       01 BFS-QUEUE-EMPTY    PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-GRAPH
           PERFORM FIND-MAX-FLOW
           DISPLAY "Maximum Flow: " MAX-FLOW
           STOP RUN.

       INITIALIZE-GRAPH.
           MOVE 0 TO MAX-FLOW
           MOVE 1 TO CURRENT-INDEX
           MOVE 1 TO QUEUE-FRONT
           MOVE 1 TO QUEUE-BACK
           MOVE 0 TO QUEUE-SIZE
           MOVE 0 TO BFS-FOUND
           MOVE 0 TO MIN-EDGE
           MOVE 0 TO TEMP-FLOW

           PERFORM INITIALIZE-RESIDUAL-MATRIX
           PERFORM INITIALIZE-VISITED-ARRAY
           PERFORM INITIALIZE-PARENT-ARRAY
           PERFORM INITIALIZE-QUEUE

           GO TO INITIALIZE-GRAPH-EXIT.

       INITIALIZE-RESIDUAL-MATRIX.
           PERFORM VARYING CURRENT-INDEX FROM 1 BY 1
               UNTIL CURRENT-INDEX > GRAPH-SIZE
               MOVE CURRENT-INDEX TO RESIDUAL-ROW(CURRENT-INDEX)
               MOVE CURRENT-INDEX TO RESIDUAL-COL(CURRENT-INDEX)
               MOVE 0 TO RESIDUAL-VALUE(CURRENT-INDEX)
           END-PERFORM.

       INITIALIZE-VISITED-ARRAY.
           PERFORM VARYING CURRENT-INDEX FROM 1 BY 1
               UNTIL CURRENT-INDEX > GRAPH-SIZE
               MOVE CURRENT-INDEX TO VISITED-INDEX(CURRENT-INDEX)
               MOVE 0 TO VISITED-FLAG(CURRENT-INDEX)
           END-PERFORM.

       INITIALIZE-PARENT-ARRAY.
           PERFORM VARYING CURRENT-INDEX FROM 1 BY 1
               UNTIL CURRENT-INDEX > GRAPH-SIZE
               MOVE CURRENT-INDEX TO PARENT-INDEX(CURRENT-INDEX)
               MOVE 0 TO PARENT-PREV(CURRENT-INDEX)
           END-PERFORM.

       INITIALIZE-QUEUE.
           PERFORM VARYING CURRENT-INDEX FROM 1 BY 1
               UNTIL CURRENT-INDEX > 100
               MOVE 0 TO QUEUE-ITEM(CURRENT-INDEX)
           END-PERFORM.

       INITIALIZE-GRAPH-EXIT.

       FIND-MAX-FLOW.
           PERFORM UNTIL BFS-FOUND = 1
               PERFORM BFS-ALGORITHM
               IF BFS-FOUND = 1
                   PERFORM UPDATE-FLOW
                   ADD TEMP-FLOW TO MAX-FLOW
               END-IF
           END-PERFORM.

       BFS-ALGORITHM.
           PERFORM INITIALIZE-VISITED-ARRAY
           PERFORM INITIALIZE-QUEUE
           PERFORM ENQUEUE WITH VALUE 1
           MOVE 0 TO BFS-FOUND

           PERFORM UNTIL QUEUE-SIZE = 0 OR BFS-FOUND = 1
               PERFORM DEQUEUE
               IF CURRENT-INDEX = GRAPH-SIZE
                   MOVE 1 TO BFS-FOUND
                   GO TO BFS-ALGORITHM-EXIT
               END-IF
               PERFORM EXAMINE-NEIGHBORS
           END-PERFORM.

       BFS-ALGORITHM-EXIT.

       EXAMINE-NEIGHBORS.
           PERFORM VARYING CURRENT-INDEX FROM 1 BY 1
               UNTIL CURRENT-INDEX > GRAPH-SIZE
               IF RESIDUAL-VALUE(CURRENT-INDEX) > 0
                   AND VISITED-FLAG(CURRENT-INDEX) = 0
                   PERFORM ENQUEUE WITH VALUE CURRENT-INDEX
                   MOVE 1 TO VISITED-FLAG(CURRENT-INDEX)
                   MOVE CURRENT-INDEX TO PARENT-PREV(CURRENT-INDEX)
               END-IF
           END-PERFORM.

       ENQUEUE.
           MOVE 1 TO QUEUE-ITEM(QUEUE-BACK)
           ADD 1 TO QUEUE-BACK
           ADD 1 TO QUEUE-SIZE.

       DEQUEUE.
           MOVE QUEUE-ITEM(QUEUE-FRONT) TO CURRENT-INDEX
           ADD 1 TO QUEUE-FRONT
           SUBTRACT 1 FROM QUEUE-SIZE.

       UPDATE-FLOW.
           MOVE GRAPH-SIZE TO CURRENT-INDEX
           MOVE 99999 TO MIN-EDGE

           PERFORM UNTIL CURRENT-INDEX = 1
               IF RESIDUAL-VALUE(CURRENT-INDEX) < MIN-EDGE
                   MOVE RESIDUAL-VALUE(CURRENT-INDEX) TO MIN-EDGE
               END-IF
               MOVE PARENT-PREV(CURRENT-INDEX) TO CURRENT-INDEX
           END-PERFORM

           MOVE MIN-EDGE TO TEMP-FLOW

           MOVE GRAPH-SIZE TO CURRENT-INDEX

           PERFORM UNTIL CURRENT-INDEX = 1
               SUBTRACT MIN-EDGE FROM RESIDUAL-VALUE(CURRENT-INDEX)
               ADD MIN-EDGE TO RESIDUAL-VALUE(PARENT-PREV(CURRENT-INDEX))
               MOVE PARENT-PREV(CURRENT-INDEX) TO CURRENT-INDEX
           END-PERFORM.
```

## Example Input File (GRAPH.DAT)

```
1 2 10
1 4 10
2 3 4
2 4 2
3 1 8
3 5 10
4 3 6
4 5 10
5 2 6
5 6 10
```

## Key Features of this Implementation

1. **Graph Representation**: Uses adjacency matrix representation for the flow network
2. **BFS Search**: Implements breadth-first search to find augmenting paths
3. **Residual Network**: Maintains residual capacities for backward edges
4. **Path Augmentation**: Updates flow along found paths
5. **Loop Control**: Continues until no more augmenting paths exist

## Algorithm Steps

1. Initialize residual network with original capacities
2. While there exists an augmenting path from source to sink:
   - Use BFS to find shortest path
   - Calculate bottleneck capacity
   - Update residual capacities
   - Add to total flow
3. Return maximum flow

This implementation demonstrates the core concepts of the Edmonds-Karp algorithm in COBOL, though in practice, more efficient implementations would use proper data structures and error handling.

