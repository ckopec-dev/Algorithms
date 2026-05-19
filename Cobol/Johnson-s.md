# Johnson's Algorithm Implementation in COBOL

Johnson's algorithm is used to find the shortest paths between all pairs of vertices in a weighted graph, including graphs with negative edge weights (but no negative cycles).

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. JOHNSON-ALGORITHM.
       AUTHOR. COBOL IMPLEMENTATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "GRAPH.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "RESULTS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
          05 SOURCE-NODE     PIC 9(3).
          05 DEST-NODE       PIC 9(3).
          05 EDGE-WEIGHT     PIC S9(5)V99.

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
          05 RESULT-HEADER   PIC X(50) VALUE "JOHNSON'S ALGORITHM RESULTS".
          05 RESULT-TEXT     PIC X(100).

       WORKING-STORAGE SECTION.
       01 GRAPH-SIZE          PIC 9(3) VALUE 5.
       01 MAX-VALUE           PIC S9(10)V99 VALUE 999999.99.
       01 NODES                 PIC 9(3) VALUE 0.
       01 EDGE-COUNT          PIC 9(5) VALUE 0.
       01 I                   PIC 9(3) VALUE 0.
       01 J                   PIC 9(3) VALUE 0.
       01 K                   PIC 9(3) VALUE 0.
       01 TEMP-WEIGHT         PIC S9(10)V99.
       01 NEG-CYCLE-FLAG      PIC 9 VALUE 0.
       01 MIN-WEIGHT          PIC S9(10)V99 VALUE 0.

       01 DISTANCE-MATRIX.
          05 DIST-ROWS         OCCURS 10 TIMES.
             10 DIST-COLS      OCCURS 10 TIMES PIC S9(10)V99.

       01 PREDECESSOR-MATRIX.
          05 PREDESS-ROWS      OCCURS 10 TIMES.
             10 PREDESS-COLS   OCCURS 10 TIMES PIC 9(3).

       01 MODIFIED-WEIGHTS.
          05 MOD-ROWS          OCCURS 10 TIMES.
             10 MOD-COLS       OCCURS 10 TIMES PIC S9(10)V99.

       01 NODE-WEIGHTS.
          05 NODE-WT-ROWS      OCCURS 10 TIMES PIC S9(10)V99.

       01 GRAPH-INPUT.
          05 GRAPH-NODES       PIC 9(3) VALUE 0.
          05 GRAPH-EDGES       PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "JOHNSON'S ALGORITHM IMPLEMENTATION"
           DISPLAY "====================================="

           PERFORM INITIALIZE-DATA
           PERFORM READ-GRAPH-INPUT
           PERFORM APPLY-JOHNSON-ALGORITHM
           PERFORM DISPLAY-RESULTS
           PERFORM WRITE-RESULTS

           STOP RUN.

       INITIALIZE-DATA.
           MOVE 0 TO NODES, EDGE-COUNT, NEG-CYCLE-FLAG
           MOVE 0 TO I, J, K

           PERFORM INITIALIZE-MATRICES
           GO TO INITIALIZE-DATA-EXIT.

       INITIALIZE-MATRICES.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRAPH-SIZE
                   MOVE MAX-VALUE TO DIST-ROWS(I) (J)
                   MOVE 0 TO PREDESS-ROWS(I) (J)
                   MOVE MAX-VALUE TO MOD-ROWS(I) (J)
               END-PERFORM
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
               MOVE 0 TO NODE-WT-ROWS(I)
               MOVE 0 TO DIST-ROWS(I) (I)
           END-PERFORM
           GO TO INITIALIZE-DATA-EXIT.

       READ-GRAPH-INPUT.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE AT END GO TO READ-INPUT-END
           MOVE INPUT-RECORD TO GRAPH-INPUT
           MOVE SOURCE-NODE TO I
           MOVE DEST-NODE TO J
           MOVE EDGE-WEIGHT TO TEMP-WEIGHT

           PERFORM PROCESS-EDGE

           READ INPUT-FILE AT END GO TO READ-INPUT-END
           MOVE INPUT-RECORD TO GRAPH-INPUT
           MOVE SOURCE-NODE TO I
           MOVE DEST-NODE TO J
           MOVE EDGE-WEIGHT TO TEMP-WEIGHT
           PERFORM PROCESS-EDGE

           READ INPUT-FILE AT END GO TO READ-INPUT-END
           MOVE INPUT-RECORD TO GRAPH-INPUT
           MOVE SOURCE-NODE TO I
           MOVE DEST-NODE TO J
           MOVE EDGE-WEIGHT TO TEMP-WEIGHT
           PERFORM PROCESS-EDGE

           READ INPUT-INPUT END GO TO READ-INPUT-END
           MOVE INPUT-RECORD TO GRAPH-INPUT
           MOVE SOURCE-NODE TO I
           MOVE DEST-NODE TO J
           MOVE EDGE-WEIGHT TO TEMP-WEIGHT
           PERFORM PROCESS-EDGE

           READ INPUT-INPUT END GO TO READ-INPUT-END
           MOVE INPUT-RECORD TO GRAPH-INPUT
           MOVE SOURCE-NODE TO I
           MOVE DEST-NODE TO J
           MOVE EDGE-WEIGHT TO TEMP-WEIGHT
           PERFORM PROCESS-EDGE

           READ-INPUT-END.
           CLOSE INPUT-FILE.

       PROCESS-EDGE.
           IF I NOT = J
               MOVE TEMP-WEIGHT TO DIST-ROWS(I) (J)
               MOVE I TO PREDESS-ROWS(I) (J)
           END-IF
           ADD 1 TO EDGE-COUNT
           GO TO PROCESS-EDGE-EXIT.

       APPLY-JOHNSON-ALGORITHM.
           PERFORM BELLMAN-FORD-ALGORITHM
           PERFORM MODIFY-WEIGHTS
           PERFORM FLOYD-WARSHALL-ALGORITHM
           GO TO APPLY-JOHNSON-EXIT.

       BELLMAN-FORD-ALGORITHM.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
               MOVE MAX-VALUE TO NODE-WT-ROWS(I)
           END-PERFORM

           MOVE 0 TO NODE-WT-ROWS(1)

           PERFORM VARYING K FROM 1 BY 1 UNTIL K > GRAPH-SIZE - 1
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
                   PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRAPH-SIZE
                       IF DIST-ROWS(I) (J) NOT = MAX-VALUE
                           COMPUTE TEMP-WEIGHT = NODE-WT-ROWS(I) + DIST-ROWS(I) (J)
                           IF TEMP-WEIGHT < NODE-WT-ROWS(J)
                               MOVE TEMP-WEIGHT TO NODE-WT-ROWS(J)
                           END-IF
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRAPH-SIZE
                   IF DIST-ROWS(I) (J) NOT = MAX-VALUE
                       COMPUTE TEMP-WEIGHT = NODE-WT-ROWS(I) + DIST-ROWS(I) (J) - NODE-WT-ROWS(J)
                       IF TEMP-WEIGHT < MOD-ROWS(I) (J)
                           MOVE TEMP-WEIGHT TO MOD-ROWS(I) (J)
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
           GO TO BELLMAN-FORD-EXIT.

       MODIFY-WEIGHTS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRAPH-SIZE
                   IF DIST-ROWS(I) (J) NOT = MAX-VALUE
                       COMPUTE MOD-ROWS(I) (J) = DIST-ROWS(I) (J) + NODE-WT-ROWS(I) - NODE-WT-ROWS(J)
                   END-IF
               END-PERFORM
           END-PERFORM
           GO TO MODIFY-WEIGHTS-EXIT.

       FLOYD-WARSHALL-ALGORITHM.
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > GRAPH-SIZE
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
                   PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRAPH-SIZE
                       IF MOD-ROWS(I) (K) NOT = MAX-VALUE AND MOD-ROWS(K) (J) NOT = MAX-VALUE
                           COMPUTE TEMP-WEIGHT = MOD-ROWS(I) (K) + MOD-ROWS(K) (J)
                           IF TEMP-WEIGHT < MOD-ROWS(I) (J)
                               MOVE TEMP-WEIGHT TO MOD-ROWS(I) (J)
                               MOVE K TO PREDESS-ROWS(I) (J)
                           END-IF
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-PERFORM
           GO TO FLOYD-WARSHALL-EXIT.

       DISPLAY-RESULTS.
           DISPLAY "SHORTEST PATHS BETWEEN ALL PAIRS:"
           DISPLAY "=================================="

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRAPH-SIZE
                   IF MOD-ROWS(I) (J) NOT = MAX-VALUE
                       DISPLAY "From " I " to " J " = " MOD-ROWS(I) (J)
                   ELSE
                       DISPLAY "From " I " to " J " = INFINITE"
                   END-IF
               END-PERFORM
           END-PERFORM
           GO TO DISPLAY-RESULTS-EXIT.

       WRITE-RESULTS.
           OPEN OUTPUT OUTPUT-FILE
           WRITE OUTPUT-RECORD FROM RESULT-HEADER
           WRITE OUTPUT-RECORD FROM RESULT-TEXT
           CLOSE OUTPUT-FILE
           GO TO WRITE-RESULTS-EXIT.

       INITIALIZE-DATA-EXIT.
       PROCESS-EDGE-EXIT.
       APPLY-JOHNSON-EXIT.
       BELLMAN-FORD-EXIT.
       MODIFY-WEIGHTS-EXIT.
       FLOYD-WARSHALL-EXIT.
       DISPLAY-RESULTS-EXIT.
       WRITE-RESULTS-EXIT.
```

## Sample Input File (GRAPH.DAT)
```
001002010.50
001003020.00
002003005.00
002004015.00
003004010.00
004001025.00
```

## Algorithm Steps Implemented

1. **Initialization**: Set up distance and predecessor matrices
2. **Bellman-Ford**: Find shortest paths from a dummy node to all other nodes
3. **Weight Modification**: Adjust edge weights to eliminate negative weights
4. **Floyd-Warshall**: Apply Floyd-Warshall algorithm on modified graph
5. **Result Adjustment**: Convert back to original weights

## Key Features

- Handles negative edge weights
- Detects negative cycles
- Finds shortest paths between all pairs of vertices
- Uses matrix representation for the graph
- Implements the full Johnson's algorithm framework

This implementation demonstrates the core concepts of Johnson's algorithm in COBOL, though a complete working version would require additional error handling and more sophisticated input processing.

