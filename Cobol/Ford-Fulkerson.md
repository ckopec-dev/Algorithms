# Ford-Fulkerson Algorithm in COBOL

Here's a complete implementation of the Ford-Fulkerson algorithm for finding the maximum flow in a flow network using COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FORD-FULKERSON.
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
          05 SOURCE-INDEX    PIC 9(3).
          05 DEST-INDEX      PIC 9(3).
          05 CAPACITY        PIC 9(5).

       WORKING-STORAGE SECTION.
       01 GRAPH-SIZE         PIC 9(3) VALUE 6.
       01 SOURCE-NODE        PIC 9(3) VALUE 1.
       01 SINK-NODE          PIC 9(3) VALUE 6.
       01 MAX-FLOW           PIC 9(10) VALUE 0.
       01 PATH-FLOW          PIC 9(10) VALUE 0.
       01 VISITED-TABLE.
          05 VISITED-ARRAY    PIC X(100) VALUE SPACES.
       01 FLOW-TABLE.
          05 FLOW-ROWS        OCCURS 100 TIMES.
             10 FLOW-COLS     PIC 9(5) VALUE 0.
       01 RESIDUAL-TABLE.
          05 RESIDUAL-ROWS    OCCURS 100 TIMES.
             10 RESIDUAL-COLS PIC 9(5) VALUE 0.
       01 PATH-TABLE.
          05 PATH-ARRAY       PIC 9(3) OCCURS 100 TIMES.
       01 QUEUE-TABLE.
          05 QUEUE-ARRAY      PIC 9(3) OCCURS 100 TIMES.
       01 QUEUE-FRONT        PIC 9(3) VALUE 1.
       01 QUEUE-BACK         PIC 9(3) VALUE 0.
       01 QUEUE-SIZE         PIC 9(3) VALUE 0.
       01 FOUND-PATH         PIC X VALUE 'N'.
       01 TEMP-INDEX         PIC 9(3).
       01 TEMP-CAPACITY      PIC 9(5).
       01 TEMP-PATH-FLOW     PIC 9(10).
       01 EOF-FLAG           PIC X VALUE 'N'.
       01 I, J, K            PIC 9(3).
       01 LOOP-CTR           PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "Ford-Fulkerson Maximum Flow Algorithm"
           DISPLAY "====================================="

           PERFORM INITIALIZE-GRAPH
           PERFORM READ-INPUT-DATA
           PERFORM BUILD-RESIDUAL-NETWORK
           PERFORM FIND-MAXIMUM-FLOW
           PERFORM DISPLAY-RESULTS

           STOP RUN.

       INITIALIZE-GRAPH.
           MOVE 0 TO MAX-FLOW
           MOVE 0 TO PATH-FLOW
           MOVE 'N' TO FOUND-PATH
           MOVE 1 TO QUEUE-FRONT
           MOVE 0 TO QUEUE-BACK
           MOVE 0 TO QUEUE-SIZE

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
               MOVE 'N' TO VISITED-ARRAY(I)
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRAPH-SIZE
                   MOVE 0 TO FLOW-COLS(I-J)
                   MOVE 0 TO RESIDUAL-COLS(I-J)
               END-PERFORM
           END-PERFORM.

       READ-INPUT-DATA.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE AT END MOVE 'Y' TO EOF-FLAG
           PERFORM UNTIL EOF-FLAG = 'Y'
               IF SOURCE-INDEX > 0 AND DEST-INDEX > 0 AND CAPACITY > 0
                   MOVE CAPACITY TO RESIDUAL-COLS(SOURCE-INDEX-DEST-INDEX)
                   MOVE CAPACITY TO FLOW-COLS(SOURCE-INDEX-DEST-INDEX)
               END-IF
               READ INPUT-FILE AT END MOVE 'Y' TO EOF-FLAG
           END-PERFORM
           CLOSE INPUT-FILE.

       BUILD-RESIDUAL-NETWORK.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRAPH-SIZE
                   IF RESIDUAL-COLS(I-J) > 0
                       IF RESIDUAL-COLS(J-I) = 0
                           MOVE RESIDUAL-COLS(I-J) TO RESIDUAL-COLS(J-I)
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM.

       FIND-MAXIMUM-FLOW.
           PERFORM UNTIL FOUND-PATH = 'N'
               PERFORM RESET-VISITED-ARRAY
               PERFORM BFS-FIND-PATH
               IF FOUND-PATH = 'Y'
                   PERFORM CALCULATE-PATH-FLOW
                   PERFORM UPDATE-FLOW
                   ADD PATH-FLOW TO MAX-FLOW
               END-IF
           END-PERFORM.

       RESET-VISITED-ARRAY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
               MOVE 'N' TO VISITED-ARRAY(I)
           END-PERFORM.

       BFS-FIND-PATH.
           MOVE 'N' TO FOUND-PATH
           MOVE 1 TO QUEUE-FRONT
           MOVE 0 TO QUEUE-BACK
           MOVE 0 TO QUEUE-SIZE

           MOVE SOURCE-NODE TO QUEUE-ARRAY(1)
           MOVE 'Y' TO VISITED-ARRAY(SOURCE-NODE)
           MOVE 1 TO QUEUE-BACK
           MOVE 1 TO QUEUE-SIZE

           PERFORM UNTIL QUEUE-SIZE = 0 OR FOUND-PATH = 'Y'
               MOVE QUEUE-ARRAY(QUEUE-FRONT) TO TEMP-INDEX
               ADD 1 TO QUEUE-FRONT
               SUBTRACT 1 FROM QUEUE-SIZE

               PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRAPH-SIZE
                   IF RESIDUAL-COLS(TEMP-INDEX-J) > 0
                       AND VISITED-ARRAY(J) = 'N'
                       MOVE 'Y' TO VISITED-ARRAY(J)
                       ADD 1 TO QUEUE-BACK
                       MOVE J TO QUEUE-ARRAY(QUEUE-BACK)
                       MOVE 1 TO QUEUE-SIZE

                       IF J = SINK-NODE
                           MOVE 'Y' TO FOUND-PATH
                           GO TO BFS-FIND-PATH-EXIT
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM.

       BFS-FIND-PATH-EXIT.
           CONTINUE.

       CALCULATE-PATH-FLOW.
           MOVE 99999 TO PATH-FLOW
           MOVE SINK-NODE TO TEMP-INDEX

           PERFORM UNTIL TEMP-INDEX = SOURCE-NODE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRAPH-SIZE
                   IF RESIDUAL-COLS(J-TEMP-INDEX) > 0
                       AND VISITED-ARRAY(J) = 'Y'
                       IF RESIDUAL-COLS(J-TEMP-INDEX) < PATH-FLOW
                           MOVE RESIDUAL-COLS(J-TEMP-INDEX) TO PATH-FLOW
                       END-IF
                       MOVE J TO TEMP-INDEX
                       GO TO CALCULATE-PATH-FLOW-CONTINUE
                   END-IF
               END-PERFORM
           END-PERFORM.

       CALCULATE-PATH-FLOW-CONTINUE.
           CONTINUE.

       UPDATE-FLOW.
           MOVE SINK-NODE TO TEMP-INDEX

           PERFORM UNTIL TEMP-INDEX = SOURCE-NODE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRAPH-SIZE
                   IF RESIDUAL-COLS(J-TEMP-INDEX) > 0
                       AND VISITED-ARRAY(J) = 'Y'
                       SUBTRACT PATH-FLOW FROM RESIDUAL-COLS(J-TEMP-INDEX)
                       ADD PATH-FLOW TO RESIDUAL-COLS(TEMP-INDEX-J)
                       MOVE J TO TEMP-INDEX
                       GO TO UPDATE-FLOW-CONTINUE
                   END-IF
               END-PERFORM
           END-PERFORM.

       UPDATE-FLOW-CONTINUE.
           CONTINUE.

       DISPLAY-RESULTS.
           DISPLAY "Maximum Flow Value: " MAX-FLOW
           DISPLAY "====================================="
           DISPLAY "Flow Network Analysis Complete"
           DISPLAY "=====================================".

       END PROGRAM FORD-FULKERSON.
```

## Sample Input Data (GRAPH.DAT)

```
1 2 10
1 4 10
2 3 4
2 4 2
3 1 8
3 5 10
4 3 6
4 5 10
5 6 10
6 3 10
```

## Algorithm Explanation

This COBOL implementation follows the Ford-Fulkerson method with the following key components:

1. **Graph Representation**: Uses adjacency matrices for flow and residual capacity tables
2. **BFS Search**: Implements breadth-first search to find augmenting paths
3. **Path Augmentation**: Updates flow along found paths using the minimum capacity
4. **Residual Network**: Maintains residual capacities for reverse edges

## Key Features

- **Modular Design**: Separate procedures for initialization, data reading, and flow computation
- **Queue Implementation**: BFS uses a simple array-based queue
- **Visited Tracking**: Prevents cycles in path finding
- **Dynamic Updates**: Continuously updates residual network after each augmentation

## Time Complexity

- **Time**: O(E × MaxFlow) where E is the number of edges
- **Space**: O(V²) for the adjacency matrices

This implementation demonstrates how classic graph algorithms can be adapted for COBOL's structured programming approach while maintaining the algorithmic correctness of the Ford-Fulkerson method.

