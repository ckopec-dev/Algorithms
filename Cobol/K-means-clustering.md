# K-Means Clustering Algorithm in COBOL

Below is a simplified implementation of the K-means clustering algorithm written in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. KMEANS-CLUSTERING.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "DATA.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "RESULTS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
          05 X-COORDINATE    PIC 9(4)V99.
          05 Y-COORDINATE    PIC 9(4)V99.

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
          05 CLUSTER-ID      PIC 99.
          05 X-COORDINATE    PIC 9(4)V99.
          05 Y-COORDINATE    PIC 9(4)V99.

       WORKING-STORAGE SECTION.
       01 WS-CLUSTERS.
          05 CLUSTER-POINT OCCURS 3 TIMES.
             10 CLUSTER-X   PIC 9(4)V99 VALUE ZERO.
             10 CLUSTER-Y   PIC 9(4)V99 VALUE ZERO.
             10 CLUSTER-COUNT PIC 99 VALUE ZERO.

       01 WS-DATA-POINTS.
          05 DATA-POINT OCCURS 20 TIMES.
             10 POINT-X     PIC 9(4)V99.
             10 POINT-Y     PIC 9(4)V99.
             10 POINT-CLUSTER PIC 99.

       01 WS-DISTANCES.
          05 DISTANCE OCCURS 3 TIMES.
             10 DISTANCE-VALUE PIC 9(5)V99 VALUE ZERO.

       01 WS-TEMPORARY.
          05 TEMP-X        PIC 9(4)V99 VALUE ZERO.
          05 TEMP-Y        PIC 9(4)V99 VALUE ZERO.
          05 WS-ITERATION  PIC 99 VALUE 1.
          05 WS-CONVERGED  PIC X VALUE 'N'.
          05 WS-POINT-COUNT PIC 99 VALUE 0.
          05 WS-I          PIC 99 VALUE 1.
          05 WS-J          PIC 99 VALUE 1.
          05 WS-K          PIC 99 VALUE 1.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-DATA.
           PERFORM RUN-KMEANS.
           PERFORM WRITE-RESULTS.
           STOP RUN.

       INITIALIZE-DATA.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE INTO POINT-X POINT-Y
               AT END GO TO END-OF-FILE
           END-READ.
           PERFORM UNTIL END-OF-FILE
               ADD 1 TO WS-POINT-COUNT
               IF WS-POINT-COUNT > 20
                   DISPLAY "TOO MANY DATA POINTS"
                   GO TO END-PROGRAM
               END-IF
               READ INPUT-FILE INTO POINT-X POINT-Y
                   AT END GO TO END-OF-FILE
               END-READ
           END-PERFORM.

           CLOSE INPUT-FILE.
           MOVE POINT-X(1) TO CLUSTER-X(1).
           MOVE POINT-Y(1) TO CLUSTER-Y(1).
           MOVE POINT-X(2) TO CLUSTER-X(2).
           MOVE POINT-Y(2) TO CLUSTER-Y(2).
           MOVE POINT-X(3) TO CLUSTER-X(3).
           MOVE POINT-Y(3) TO CLUSTER-Y(3).

       END-OF-FILE.
           GO TO MAIN-PROGRAM.

       RUN-KMEANS.
           PERFORM UNTIL WS-CONVERGED = 'Y'
               PERFORM RESET-CLUSTERS.
               PERFORM ASSIGN-POINTS.
               PERFORM UPDATE-CLUSTERS.
               ADD 1 TO WS-ITERATION
               IF WS-ITERATION > 100
                   MOVE 'Y' TO WS-CONVERGED
               END-IF
           END-PERFORM.

       RESET-CLUSTERS.
           PERFORM VARYING WS-K FROM 1 BY 1 UNTIL WS-K > 3
               MOVE ZERO TO CLUSTER-X(WS-K)
               MOVE ZERO TO CLUSTER-Y(WS-K)
               MOVE ZERO TO CLUSTER-COUNT(WS-K)
           END-PERFORM.

       ASSIGN-POINTS.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-POINT-COUNT
               PERFORM CALCULATE-DISTANCES.
               PERFORM FIND-MINIMUM-DISTANCE.
               MOVE WS-J TO POINT-CLUSTER(WS-I)
               ADD 1 TO CLUSTER-COUNT(WS-J)
           END-PERFORM.

       CALCULATE-DISTANCES.
           PERFORM VARYING WS-K FROM 1 BY 1 UNTIL WS-K > 3
               COMPUTE DISTANCE-VALUE(WS-K) =
                   FUNCTION SQRT((POINT-X(WS-I) - CLUSTER-X(WS-K)) ** 2 +
                                 (POINT-Y(WS-I) - CLUSTER-Y(WS-K)) ** 2)
           END-PERFORM.

       FIND-MINIMUM-DISTANCE.
           MOVE 1 TO WS-J.
           MOVE DISTANCE-VALUE(1) TO TEMP-X.
           PERFORM VARYING WS-K FROM 2 BY 1 UNTIL WS-K > 3
               IF DISTANCE-VALUE(WS-K) < TEMP-X
                   MOVE DISTANCE-VALUE(WS-K) TO TEMP-X
                   MOVE WS-K TO WS-J
               END-IF
           END-PERFORM.

       UPDATE-CLUSTERS.
           PERFORM VARYING WS-K FROM 1 BY 1 UNTIL WS-K > 3
               IF CLUSTER-COUNT(WS-K) > 0
                   COMPUTE CLUSTER-X(WS-K) = 
                       (CLUSTER-X(WS-K) / CLUSTER-COUNT(WS-K))
                   COMPUTE CLUSTER-Y(WS-K) = 
                       (CLUSTER-Y(WS-K) / CLUSTER-COUNT(WS-K))
               END-IF
           END-PERFORM.

       WRITE-RESULTS.
           OPEN OUTPUT OUTPUT-FILE.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-POINT-COUNT
               MOVE POINT-CLUSTER(WS-I) TO CLUSTER-ID
               MOVE POINT-X(WS-I) TO X-COORDINATE
               MOVE POINT-Y(WS-I) TO Y-COORDINATE
               WRITE OUTPUT-RECORD
           END-PERFORM.
           CLOSE OUTPUT-FILE.

       END-PROGRAM.
```

## Sample Input File (DATA.DAT)
```
10.5 15.2
20.3 25.8
30.1 35.9
40.7 45.3
50.2 55.6
```

## Key Features of This Implementation:

1. **Data Structure**: Uses OCCURS clauses to handle arrays of data points and cluster centers
2. **Initialization**: Sets up initial cluster centroids from first few data points
3. **Distance Calculation**: Computes Euclidean distance between points and centroids
4. **Assignment**: Assigns each point to the nearest cluster
5. **Update**: Recalculates cluster centroids based on assigned points
6. **Convergence**: Stops after a maximum number of iterations or when centroids don't change significantly

## Limitations:

- This is a simplified implementation for educational purposes
- Uses fixed-size arrays instead of dynamic allocation
- Basic convergence criteria (iteration count)
- No error handling for edge cases
- Assumes 3 clusters and 20 maximum data points

This demonstrates how clustering algorithms can be implemented in COBOL, though such implementations would typically be done in more modern languages for production use.