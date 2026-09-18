# Lloyd's Algorithm in COBOL

Lloyd's algorithm (K-means clustering) implemented in COBOL. This example demonstrates the basic structure of the algorithm with sample data.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LLOYDS-ALGORITHM.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATA-FILE ASSIGN TO "DATA.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CENTROIDS-FILE ASSIGN TO "CENTROIDS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  DATA-FILE.
       01  DATA-RECORD.
           05  X-COORD      PIC 9(3)V99.
           05  Y-COORD      PIC 9(3)V99.

       FD  CENTROIDS-FILE.
       01  CENTROID-RECORD.
           05  CENTROID-ID  PIC 9.
           05  CENTROID-X   PIC 9(3)V99.
           05  CENTROID-Y   PIC 9(3)V99.

       WORKING-STORAGE SECTION.
       01  WS-LOOP-COUNTER     PIC 99 VALUE 0.
       01  WS-ITERATION        PIC 99 VALUE 0.
       01  WS-MAX-ITERATIONS   PIC 99 VALUE 100.
       01  WS-CONVERGENCE-TOLERANCE PIC 9V99 VALUE 0.01.
       01  WS-DISTANCE         PIC 9(3)V99.
       01  WS-MIN-DISTANCE     PIC 9(3)V99.
       01  WS-CLUSTER-ID       PIC 9.
       01  WS-SUM-X            PIC 9(5)V99 VALUE 0.
       01  WS-SUM-Y            PIC 9(5)V99 VALUE 0.
       01  WS-COUNT            PIC 9(5) VALUE 0.
       01  WS-CONVERGED        PIC X VALUE 'N'.
       01  WS-END-OF-FILE      PIC X VALUE 'N'.

       01  DATA-ARRAY.
           05  DATA-ITEM OCCURS 100 TIMES.
               10  ITEM-X   PIC 9(3)V99.
               10  ITEM-Y   PIC 9(3)V99.
               10  ITEM-CLUSTER PIC 9.

       01  CENTROIDS-ARRAY.
           05  CENTROID-ITEM OCCURS 3 TIMES.
               10  CENT-X   PIC 9(3)V99.
               10  CENT-Y   PIC 9(3)V99.

       01  TEMP-DATA-ARRAY.
           05  TEMP-ITEM OCCURS 100 TIMES.
               10  TEMP-X   PIC 9(3)V99.
               10  TEMP-Y   PIC 9(3)V99.
               10  TEMP-CLUSTER PIC 9.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "LLOYD'S ALGORITHM - K-MEANS CLUSTERING"
           DISPLAY "======================================"

           PERFORM INITIALIZE-DATA
           PERFORM INITIALIZE-CENTROIDS
           PERFORM ITERATE-CLUSTERING

           DISPLAY "FINAL CLUSTERS:"
           PERFORM DISPLAY-RESULTS

           STOP RUN.

       INITIALIZE-DATA.
           OPEN INPUT DATA-FILE
           MOVE 0 TO WS-LOOP-COUNTER
           PERFORM UNTIL WS-LOOP-COUNTER > 50
               READ DATA-FILE INTO DATA-ITEM(WS-LOOP-COUNTER)
                   AT END MOVE 'Y' TO WS-END-OF-FILE
                   NOT AT END ADD 1 TO WS-LOOP-COUNTER
               END-READ
           END-PERFORM
           CLOSE DATA-FILE.

       INITIALIZE-CENTROIDS.
           DISPLAY "Initializing centroids..."
           MOVE 10.00 TO CENT-X(1)
           MOVE 10.00 TO CENT-Y(1)
           MOVE 20.00 TO CENT-X(2)
           MOVE 20.00 TO CENT-Y(2)
           MOVE 30.00 TO CENT-X(3)
           MOVE 30.00 TO CENT-Y(3).

       ITERATE-CLUSTERING.
           PERFORM UNTIL WS-ITERATION > WS-MAX-ITERATIONS
               PERFORM ASSIGN-POINTS-TO-CLUSTERS
               PERFORM UPDATE-CENTROIDS
               PERFORM CHECK-CONVERGENCE
               IF WS-CONVERGED = 'Y' THEN
                   DISPLAY "Converged after " WS-ITERATION " iterations"
                   EXIT PARAGRAPH
               END-IF
               ADD 1 TO WS-ITERATION
           END-PERFORM.

       ASSIGN-POINTS-TO-CLUSTERS.
           PERFORM VARYING WS-LOOP-COUNTER FROM 1 BY 1 
               UNTIL WS-LOOP-COUNTER > 50
               MOVE 999.99 TO WS-MIN-DISTANCE
               MOVE 0 TO WS-CLUSTER-ID
               PERFORM VARYING WS-CLUSTER-ID FROM 1 BY 1 
                   UNTIL WS-CLUSTER-ID > 3
                   COMPUTE WS-DISTANCE = 
                       FUNCTION SQRT ((ITEM-X(WS-LOOP-COUNTER) - CENT-X(WS-CLUSTER-ID)) ** 2 +
                                      (ITEM-Y(WS-LOOP-COUNTER) - CENT-Y(WS-CLUSTER-ID)) ** 2)
                   IF WS-DISTANCE < WS-MIN-DISTANCE THEN
                       MOVE WS-DISTANCE TO WS-MIN-DISTANCE
                       MOVE WS-CLUSTER-ID TO ITEM-CLUSTER(WS-LOOP-COUNTER)
                   END-IF
               END-PERFORM
           END-PERFORM.

       UPDATE-CENTROIDS.
           PERFORM VARYING WS-LOOP-COUNTER FROM 1 BY 1 
               UNTIL WS-LOOP-COUNTER > 3
               MOVE 0 TO WS-SUM-X
               MOVE 0 TO WS-SUM-Y
               MOVE 0 TO WS-COUNT
               PERFORM VARYING WS-CLUSTER-ID FROM 1 BY 1 
                   UNTIL WS-CLUSTER-ID > 50
                   IF ITEM-CLUSTER(WS-CLUSTER-ID) = WS-LOOP-COUNTER THEN
                       ADD ITEM-X(WS-CLUSTER-ID) TO WS-SUM-X
                       ADD ITEM-Y(WS-CLUSTER-ID) TO WS-SUM-Y
                       ADD 1 TO WS-COUNT
                   END-IF
               END-PERFORM
               IF WS-COUNT > 0 THEN
                   COMPUTE CENT-X(WS-LOOP-COUNTER) = WS-SUM-X / WS-COUNT
                   COMPUTE CENT-Y(WS-LOOP-COUNTER) = WS-SUM-Y / WS-COUNT
               END-IF
           END-PERFORM.

       CHECK-CONVERGENCE.
           MOVE 'N' TO WS-CONVERGED
           PERFORM VARYING WS-LOOP-COUNTER FROM 1 BY 1 
               UNTIL WS-LOOP-COUNTER > 3
               IF FUNCTION ABS(CENT-X(WS-LOOP-COUNTER) - CENT-X(WS-LOOP-COUNTER)) < WS-CONVERGENCE-TOLERANCE
                  AND FUNCTION ABS(CENT-Y(WS-LOOP-COUNTER) - CENT-Y(WS-LOOP-COUNTER)) < WS-CONVERGENCE-TOLERANCE THEN
                   MOVE 'Y' TO WS-CONVERGED
               END-IF
           END-PERFORM.

       DISPLAY-RESULTS.
           DISPLAY "Cluster 1:"
           PERFORM VARYING WS-LOOP-COUNTER FROM 1 BY 1 
               UNTIL WS-LOOP-COUNTER > 50
               IF ITEM-CLUSTER(WS-LOOP-COUNTER) = 1 THEN
                   DISPLAY "  Point (" ITEM-X(WS-LOOP-COUNTER) "," ITEM-Y(WS-LOOP-COUNTER) ")"
               END-IF
           END-PERFORM

           DISPLAY "Cluster 2:"
           PERFORM VARYING WS-LOOP-COUNTER FROM 1 BY 1 
               UNTIL WS-LOOP-COUNTER > 50
               IF ITEM-CLUSTER(WS-LOOP-COUNTER) = 2 THEN
                   DISPLAY "  Point (" ITEM-X(WS-LOOP-COUNTER) "," ITEM-Y(WS-LOOP-COUNTER) ")"
               END-IF
           END-PERFORM

           DISPLAY "Cluster 3:"
           PERFORM VARYING WS-LOOP-COUNTER FROM 1 BY 1 
               UNTIL WS-LOOP-COUNTER > 50
               IF ITEM-CLUSTER(WS-LOOP-COUNTER) = 3 THEN
                   DISPLAY "  Point (" ITEM-X(WS-LOOP-COUNTER) "," ITEM-Y(WS-LOOP-COUNTER) ")"
               END-IF
           END-PERFORM.
```

## Key Components of this Implementation:

1. **Data Structures**: Arrays for data points and centroids
2. **Initialization**: Sets up initial cluster centroids
3. **Assignment Phase**: Assigns each data point to the nearest centroid
4. **Update Phase**: Recalculates centroid positions based on assigned points
5. **Convergence Check**: Determines when algorithm should stop
6. **Iteration Loop**: Repeats assignment and update until convergence

This example demonstrates the core concepts of Lloyd's algorithm in COBOL syntax, though actual implementation would require proper data files and more sophisticated convergence detection.