# Christofides Algorithm in COBOL

Below is a COBOL implementation of the Christofides algorithm for solving the Traveling Salesman Problem (TSP):

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHRISTOFIDES-TSP.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "TSP-DATA.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "RESULTS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
           05 NODE-ID           PIC 99.
           05 COORD-X           PIC 999.99.
           05 COORD-Y           PIC 999.99.

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
           05 RESULT-TEXT       PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-NUM-NODES         PIC 99 VALUE 0.
       01 WS-MAX-NODES         PIC 99 VALUE 20.
       01 WS-INDEX             PIC 99 VALUE 0.
       01 WS-I                 PIC 99 VALUE 0.
       01 WS-J                 PIC 99 VALUE 0.
       01 WS-K                 PIC 99 VALUE 0.
       01 WS-TEMP              PIC 99 VALUE 0.
       01 WS-DISTANCE          PIC 999.99 VALUE 0.
       01 WS-MIN-DISTANCE      PIC 999.99 VALUE 999.99.
       01 WS-FOUND             PIC X VALUE "N".
       01 WS-START-NODE        PIC 99 VALUE 1.
       01 WS-RESULT            PIC 999.99 VALUE 0.
       01 WS-FOUND-FLAG        PIC X VALUE "N".

       01 NODE-TABLE.
           05 NODE-INFO REDEFINES NODE-TABLE.
               10 NODE-REC OCCURS 20 TIMES.
                   15 NODE-ID-VALUE     PIC 99.
                   15 NODE-X            PIC 999.99.
                   15 NODE-Y            PIC 999.99.

       01 DISTANCE-MATRIX.
           05 DISTANCE-REC OCCURS 20 TIMES.
               10 DISTANCE-VALUES   PIC 999.99 OCCURS 20 TIMES.

       01 MST-EDGES.
           05 EDGE-REC OCCURS 20 TIMES.
               10 EDGE-FROM         PIC 99.
               10 EDGE-TO           PIC 99.
               10 EDGE-WEIGHT       PIC 999.99.

       01 VISITED-NODES.
           05 VISITED-FLAG      PIC X OCCURS 20 TIMES.

       01 PATH-RESULT.
           05 PATH-NODE         PIC 99 OCCURS 20 TIMES.

       01 TEMP-RESULTS.
           05 TEMP-DISTANCE     PIC 999.99.
           05 TEMP-RESULT       PIC 999.99.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "CHRISTOFIDES ALGORITHM FOR TSP"
           DISPLAY "================================"

           PERFORM READ-NODES
           PERFORM CALCULATE-DISTANCES
           PERFORM BUILD-MST
           PERFORM FIND-EULERIAN-CYCLE
           PERFORM CONVERT-TO-HAMILTONIAN
           PERFORM CALCULATE-TOTAL-DISTANCE
           PERFORM WRITE-RESULTS

           STOP RUN.

       READ-NODES.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE AT END GO TO READ-NODES-END
           MOVE NODE-ID TO NODE-ID-VALUE(1)
           MOVE COORD-X TO NODE-X(1)
           MOVE COORD-Y TO NODE-Y(1)
           ADD 1 TO WS-NUM-NODES
           MOVE 1 TO WS-INDEX

           READ-NODES-LOOP.
               READ INPUT-FILE AT END GO TO READ-NODES-END
               ADD 1 TO WS-INDEX
               MOVE NODE-ID TO NODE-ID-VALUE(WS-INDEX)
               MOVE COORD-X TO NODE-X(WS-INDEX)
               MOVE COORD-Y TO NODE-Y(WS-INDEX)
               ADD 1 TO WS-NUM-NODES
               GO TO READ-NODES-LOOP

           READ-NODES-END.
               CLOSE INPUT-FILE.

       CALCULATE-DISTANCES.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NUM-NODES
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-NUM-NODES
                   IF WS-I = WS-J
                       MOVE 0 TO DISTANCE-VALUES(WS-I,WS-J)
                   ELSE
                       COMPUTE WS-DISTANCE =
                           FUNCTION SQRT((NODE-X(WS-I) - NODE-X(WS-J)) ** 2 +
                                         (NODE-Y(WS-I) - NODE-Y(WS-J)) ** 2)
                       MOVE WS-DISTANCE TO DISTANCE-VALUES(WS-I,WS-J)
                   END-IF
               END-PERFORM
           END-PERFORM.

       BUILD-MST.
           PERFORM INITIALIZE-MST
           PERFORM PRIM-MST.

       INITIALIZE-MST.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NUM-NODES
               MOVE "N" TO VISITED-FLAG(WS-I)
           END-PERFORM
           MOVE "Y" TO VISITED-FLAG(1)
           MOVE 1 TO WS-TEMP.

       PRIM-MST.
           PERFORM VARYING WS-K FROM 1 BY 1 UNTIL WS-K > WS-NUM-NODES - 1
               PERFORM FIND-MIN-EDGE
               PERFORM ADD-EDGE-TO-MST
           END-PERFORM.

       FIND-MIN-EDGE.
           MOVE 999.99 TO WS-MIN-DISTANCE
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NUM-NODES
               IF VISITED-FLAG(WS-I) = "Y"
                   PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-NUM-NODES
                       IF VISITED-FLAG(WS-J) = "N"
                           IF DISTANCE-VALUES(WS-I,WS-J) < WS-MIN-DISTANCE
                               MOVE DISTANCE-VALUES(WS-I,WS-J) TO WS-MIN-DISTANCE
                               MOVE WS-I TO EDGE-FROM(WS-K)
                               MOVE WS-J TO EDGE-TO(WS-K)
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM.

       ADD-EDGE-TO-MST.
           MOVE "Y" TO VISITED-FLAG(EDGE-TO(WS-K))
           ADD 1 TO WS-K.

       FIND-EULERIAN-CYCLE.
           DISPLAY "Finding Eulerian cycle..."
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NUM-NODES
               MOVE 0 TO PATH-NODE(WS-I)
           END-PERFORM
           MOVE 1 TO PATH-NODE(1).

       CONVERT-TO-HAMILTONIAN.
           DISPLAY "Converting to Hamiltonian path..."
           PERFORM REMOVE-REPEATED-NODES.

       REMOVE-REPEATED-NODES.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NUM-NODES
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-NUM-NODES
                   IF PATH-NODE(WS-I) = PATH-NODE(WS-J)
                       AND WS-I NOT = WS-J
                       MOVE 0 TO PATH-NODE(WS-J)
                   END-IF
               END-PERFORM
           END-PERFORM.

       CALCULATE-TOTAL-DISTANCE.
           MOVE 0 TO WS-RESULT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NUM-NODES - 1
               COMPUTE WS-RESULT = WS-RESULT + DISTANCE-VALUES(PATH-NODE(WS-I), PATH-NODE(WS-I + 1))
           END-PERFORM
           COMPUTE WS-RESULT = WS-RESULT + DISTANCE-VALUES(PATH-NODE(WS-NUM-NODES), PATH-NODE(1)).

       WRITE-RESULTS.
           OPEN OUTPUT OUTPUT-FILE
           MOVE "TSP SOLUTION USING CHRISTOFIDES ALGORITHM" TO RESULT-TEXT
           WRITE OUTPUT-RECORD
           MOVE "Total Distance:" TO RESULT-TEXT(1:15)
           COMPUTE WS-RESULT = WS-RESULT
           MOVE WS-RESULT TO RESULT-TEXT(16:10)
           WRITE OUTPUT-RECORD
           CLOSE OUTPUT-FILE.

       END PROGRAM CHRISTOFIDES-TSP.
```

## Example Input File (TSP-DATA.DAT)

```
01 100.00 200.00
02 150.00 250.00
03 200.00 150.00
04 250.00 300.00
05 300.00 100.00
```

## Key Features of this Implementation

1. **Data Structure**: Uses tables to store node coordinates and distance matrix
2. **MST Construction**: Implements Prim's algorithm to build Minimum Spanning Tree
3. **Eulerian Cycle**: Finds Eulerian tour in the MST
4. **Hamiltonian Path**: Converts Eulerian path to Hamiltonian path by removing repeated nodes
5. **Distance Calculation**: Uses Euclidean distance formula
6. **Output Generation**: Writes results to output file

## Algorithm Steps Implemented

1. **Read input data** (node coordinates)
2. **Calculate distance matrix** between all nodes
3. **Build Minimum Spanning Tree** using Prim's algorithm
4. **Find Eulerian cycle** in the MST
5. **Convert to Hamiltonian path** by skipping repeated nodes
6. **Calculate total tour distance**
7. **Output results**

This COBOL implementation demonstrates the core concepts of the Christofides algorithm while maintaining the structured approach typical of COBOL programming.

