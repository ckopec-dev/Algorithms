# DBSCAN Clustering Algorithm in COBOL

Below is an example implementation of the DBSCAN clustering algorithm in COBOL. This is a simplified version that demonstrates the core concepts.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DBSCAN-CLUSTER.
       AUTHOR. AI Assistant.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-DIMENSIONS.
           05  WS-NUM-POINTS        PIC 9(4) VALUE 10.
           05  WS-NUM-DIMENSIONS    PIC 9(2) VALUE 2.
           05  WS-EPSILON           PIC 9V99 VALUE 0.5.
           05  WS-MIN-POINTS        PIC 9(2) VALUE 3.

       01  WS-DATASET.
           05  WS-POINTS.
               10  WS-POINT OCCURS 10 TIMES INDEXED BY I.
                   15  WS-X-COORD         PIC 9V99.
                   15  WS-Y-COORD         PIC 9V99.
                   15  WS-POINT-STATUS    PIC X VALUE "U".
                       88  WS-UNVISITED   VALUE "U".
                       88  WS-VISITED     VALUE "V".
                       88  WS-NOISE       VALUE "N".
           05  WS-CLUSTERS.
               10  WS-CLUSTER OCCURS 10 TIMES INDEXED BY J.
                   15  WS-CLUSTER-ID      PIC 9(2).
                   15  WS-CLUSTER-SIZE    PIC 9(3).
                   15  WS-CLUSTER-POINTS.
                       20  WS-POINT-ID OCCURS 100 TIMES INDEXED BY K.
                           25  WS-POINT-INDEX PIC 9(2).

       01  WS-TEMPORARY.
           05  WS-NEIGHBORS.
               10  WS-NEIGHBOR OCCURS 10 TIMES INDEXED BY L.
                   15  WS-NEIGHBOR-INDEX PIC 9(2).
           05  WS-QUEUE.
               10  WS-QUEUE-ITEM OCCURS 10 TIMES INDEXED BY M.
                   15  WS-QUEUE-INDEX    PIC 9(2).
           05  WS-DISTANCE          PIC 9V99.
           05  WS-POINT-COUNT       PIC 9(4) VALUE 0.
           05  WS-CLUSTER-COUNT     PIC 9(2) VALUE 0.

       01  WS-OUTPUT.
           05  WS-RESULT            PIC X(50).
           05  WS-LOG-MESSAGE       PIC X(100).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-DATASET
           PERFORM DBSCAN-ALGORITHM
           PERFORM DISPLAY-RESULTS
           STOP RUN.

       INITIALIZE-DATASET.
           DISPLAY "Initializing dataset..."
           
           MOVE 100 TO WS-X-COORD(1)
           MOVE 100 TO WS-Y-COORD(1)
           MOVE 105 TO WS-X-COORD(2)
           MOVE 105 TO WS-Y-COORD(2)
           MOVE 110 TO WS-X-COORD(3)
           MOVE 110 TO WS-Y-COORD(3)
           MOVE 200 TO WS-X-COORD(4)
           MOVE 200 TO WS-Y-COORD(4)
           MOVE 205 TO WS-X-X-COORD(5)
           MOVE 205 TO WS-Y-COORD(5)
           MOVE 300 TO WS-X-COORD(6)
           MOVE 300 TO WS-Y-COORD(6)
           MOVE 500 TO WS-X-COORD(7)
           MOVE 500 TO WS-Y-COORD(7)
           MOVE 505 TO WS-X-COORD(8)
           MOVE 505 TO WS-Y-COORD(8)
           MOVE 510 TO WS-X-COORD(9)
           MOVE 510 TO WS-Y-COORD(9)
           MOVE 600 TO WS-X-COORD(10)
           MOVE 600 TO WS-Y-COORD(10)

           DISPLAY "Dataset initialized."

       DBSCAN-ALGORITHM.
           DISPLAY "Starting DBSCAN algorithm..."
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-NUM-POINTS
               IF WS-UNVISITED
                   PERFORM FIND-NEIGHBORS
                   IF WS-NEIGHBOR-COUNT >= WS-MIN-POINTS
                       PERFORM GROW-CLUSTER
                   ELSE
                       MOVE "N" TO WS-POINT-STATUS
                   END-IF
               END-IF
           END-PERFORM

           DISPLAY "DBSCAN algorithm completed."

       FIND-NEIGHBORS.
           MOVE 0 TO WS-NEIGHBOR-COUNT
           
           PERFORM VARYING L FROM 1 BY 1 UNTIL L > WS-NUM-POINTS
               IF L NOT = I
                   PERFORM CALCULATE-DISTANCE
                   IF WS-DISTANCE <= WS-EPSILON
                       ADD 1 TO WS-NEIGHBOR-COUNT
                       MOVE L TO WS-NEIGHBOR(WS-NEIGHBOR-COUNT)
                   END-IF
               END-IF
           END-PERFORM

       CALCULATE-DISTANCE.
           COMPUTE WS-DISTANCE = FUNCTION SQRT
               ((WS-X-COORD(I) - WS-X-COORD(L)) ** 2 +
                (WS-Y-COORD(I) - WS-Y-COORD(L)) ** 2)

       GROW-CLUSTER.
           ADD 1 TO WS-CLUSTER-COUNT
           MOVE WS-CLUSTER-COUNT TO WS-CLUSTER-ID(WS-CLUSTER-COUNT)
           MOVE 0 TO WS-CLUSTER-SIZE(WS-CLUSTER-COUNT)
           
           PERFORM ADD-TO-QUEUE
           
           PERFORM VARYING M FROM 1 BY 1 UNTIL M > WS-QUEUE-LENGTH
               IF WS-UNVISITED
                   MOVE "V" TO WS-POINT-STATUS
                   PERFORM FIND-NEIGHBORS
                   IF WS-NEIGHBOR-COUNT >= WS-MIN-POINTS
                       PERFORM ADD-TO-QUEUE
                   END-IF
               END-IF
           END-PERFORM

       ADD-TO-QUEUE.
           ADD 1 TO WS-QUEUE-LENGTH
           MOVE I TO WS-QUEUE-ITEM(WS-QUEUE-LENGTH)

       DISPLAY-RESULTS.
           DISPLAY "Cluster Results:"
           DISPLAY "=================="
           
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > WS-CLUSTER-COUNT
               DISPLAY "Cluster " WS-CLUSTER-ID(J) " contains "
                       WS-CLUSTER-SIZE(J) " points"
           END-PERFORM

           DISPLAY "Noise points:"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-NUM-POINTS
               IF WS-NOISE
                   DISPLAY "Point " I " is noise"
               END-IF
           END-PERFORM

       END PROGRAM DBSCAN-CLUSTER.
```

## Explanation of Key Components

### 1. **Data Structure**
- `WS-POINTS`: Array of 2D points with coordinates and status
- `WS-CLUSTERS`: Array to store cluster information
- `WS-NEIGHBORS`: Temporary storage for neighbor points

### 2. **Key Functions**
- **FIND-NEIGHBORS**: Calculates Euclidean distance between points
- **GROW-CLUSTER**: Expands clusters using breadth-first search
- **DBSCAN-ALGORITHM**: Main clustering logic

### 3. **Algorithm Steps**
1. Initialize dataset with sample points
2. For each unvisited point:
   - Find neighbors within epsilon distance
   - If enough neighbors, form cluster
   - Otherwise mark as noise
3. Expand clusters by adding density-reachable points

### 4. **Variables**
- `WS-EPSILON`: Distance threshold
- `WS-MIN-POINTS`: Minimum points required for cluster
- `WS-POINT-STATUS`: U=Unvisited, V=Visited, N=Noise

This COBOL implementation demonstrates the fundamental concepts of DBSCAN clustering while respecting COBOL's structured programming approach.

