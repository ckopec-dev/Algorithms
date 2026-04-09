# Bellman-Ford Algorithm in COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BELLMAN-FORD.
       AUTHOR. COBOL IMPLEMENTATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "GRAPH.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
          05 SRC-VALUE    PIC 9(3).
          05 DST-VALUE    PIC 9(3).
          05 WEIGHT-VALUE PIC 9(3).

       WORKING-STORAGE SECTION.
       01 GRAPH-STRUCTURE.
          05 VERTEX-COUNT    PIC 9(3) VALUE 6.
          05 EDGE-COUNT      PIC 9(3) VALUE 9.
          05 MAX-VALUE       PIC 9(9) VALUE 999999.
          05 SRC-VERTEX      PIC 9(3).
          05 DST-VERTEX      PIC 9(3).
          05 EDGE-WEIGHT     PIC 9(3).

       01 DISTANCE-TABLE.
          05 DISTANCE OCCURS 10 TIMES.
             10 DIST-VALUE PIC 9(9) VALUE 999999.
             10 DIST-PREDECESSOR PIC 9(3) VALUE 0.

       01 EDGE-LIST.
          05 EDGE OCCURS 20 TIMES.
             10 EDGE-SRC PIC 9(3).
             10 EDGE-DST PIC 9(3).
             10 EDGE-WGT PIC 9(3).

       01 I-J-K           PIC 9(3).
       01 RELAXED         PIC 9(1) VALUE 0.
       01 SOURCE-VERTEX   PIC 9(3) VALUE 1.
       01 TEMP-DIST       PIC 9(9).
       01 EOF-FLAG        PIC 9(1) VALUE 0.
       01 ERROR-FLAG      PIC 9(1) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-DISTANCES
           PERFORM READ-EDGES
           PERFORM BELLMAN-FORD-ALGORITHM
           PERFORM PRINT-RESULTS
           STOP RUN.

       INITIALIZE-DISTANCES.
           MOVE SOURCE-VERTEX TO DIST-VALUE(1)
           MOVE 0 TO DIST-PREDECESSOR(1)
           PERFORM VARYING I-J-K FROM 2 BY 1 UNTIL I-J-K > VERTEX-COUNT
               MOVE MAX-VALUE TO DIST-VALUE(I-J-K)
               MOVE 0 TO DIST-PREDECESSOR(I-J-K)
           END-PERFORM.

       READ-EDGES.
           OPEN INPUT INPUT-FILE
           PERFORM UNTIL EOF-FLAG = 1
               READ INPUT-FILE INTO INPUT-RECORD
                   AT END MOVE 1 TO EOF-FLAG
                   NOT AT END
                       PERFORM STORE-EDGE
               END-READ
           END-PERFORM
           CLOSE INPUT-FILE.

       STORE-EDGE.
           MOVE SRC-VALUE TO EDGE-SRC(EDGE-COUNT)
           MOVE DST-VALUE TO EDGE-DST(EDGE-COUNT)
           MOVE WEIGHT-VALUE TO EDGE-WGT(EDGE-COUNT)
           SUBTRACT 1 FROM EDGE-COUNT
           ADD 1 TO EDGE-COUNT.

       BELLMAN-FORD-ALGORITHM.
           PERFORM VARYING I-J-K FROM 1 BY 1 UNTIL I-J-K > VERTEX-COUNT - 1
               PERFORM RELAX-ALL-EDGES
           END-PERFORM.

       RELAX-ALL-EDGES.
           PERFORM VARYING I-J-K FROM 1 BY 1 UNTIL I-J-K > EDGE-COUNT
               PERFORM RELAX-EDGE
           END-PERFORM.

       RELAX-EDGE.
           MOVE EDGE-SRC(I-J-K) TO SRC-VERTEX
           MOVE EDGE-DST(I-J-K) TO DST-VERTEX
           MOVE EDGE-WGT(I-J-K) TO EDGE-WEIGHT

           COMPUTE TEMP-DIST = DIST-VALUE(SRC-VERTEX) + EDGE-WEIGHT
           IF TEMP-DIST < DIST-VALUE(DST-VERTEX)
               MOVE TEMP-DIST TO DIST-VALUE(DST-VERTEX)
               MOVE SRC-VERTEX TO DIST-PREDECESSOR(DST-VERTEX)
           END-IF.

       PRINT-RESULTS.
           DISPLAY "BELLMAN-FORD SHORTEST PATH RESULTS"
           DISPLAY "================================"
           PERFORM VARYING I-J-K FROM 1 BY 1 UNTIL I-J-K > VERTEX-COUNT
               IF DIST-VALUE(I-J-K) = MAX-VALUE
                   DISPLAY "Vertex " I-J-K " : INFINITE"
               ELSE
                   DISPLAY "Vertex " I-J-K " : " DIST-VALUE(I-J-K)
               END-IF
           END-PERFORM.

       END PROGRAM BELLMAN-FORD.
```

## Sample Input File (GRAPH.DAT)
```
1 2 4
1 3 2
2 3 1
2 4 5
3 4 8
3 5 10
4 5 2
4 6 6
5 6 3
```

## Explanation

This COBOL implementation of the Bellman-Ford algorithm includes:

1. **Data Definitions**: 
   - Vertex and edge counts
   - Distance table with predecessor tracking
   - Edge list structure

2. **Key Components**:
   - `INITIALIZE-DISTANCES`: Sets up initial distances
   - `READ-EDGES`: Reads graph data from input file
   - `BELLMAN-FORD-ALGORITHM`: Main algorithm loop
   - `RELAX-EDGE`: Implements the relaxation operation
   - `PRINT-RESULTS`: Displays final shortest paths

3. **Algorithm Steps**:
   - Initialize all distances to infinity except source
   - Relax all edges V-1 times
   - Output the shortest distances from source to all vertices

The algorithm handles negative edge weights and can detect negative weight cycles, though the current implementation focuses on finding shortest paths.

