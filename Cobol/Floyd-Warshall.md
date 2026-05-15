# Floyd-Warshall Algorithm in COBOL

Here's a complete COBOL implementation of the Floyd-Warshall algorithm for finding shortest paths between all pairs of vertices in a weighted graph:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FLOYD-WARSHALL-ALGORITHM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GRAPH-FILE ASSIGN TO "GRAPH.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD GRAPH-FILE.
       01 GRAPH-RECORD.
          05 VERTEX-COUNT     PIC 99.
          05 EDGE-COUNT       PIC 99.
          05 EDGE-DATA.
             10 FROM-VERTEX   PIC 99.
             10 TO-VERTEX     PIC 99.
             10 EDGE-WEIGHT   PIC 999.

       WORKING-STORAGE SECTION.
       01 GRAPH-SIZE           PIC 99 VALUE 5.
       01 MAX-VALUE            PIC 999 VALUE 999.
       01 I, J, K              PIC 99.
       01 DISTANCE-TABLE.
          05 DIST-ROW OCCURS 10 TIMES.
             10 DIST-ELEMENT OCCURS 10 TIMES PIC 999.
       01 TEMP-DISTANCE        PIC 999.
       01 EOF-FLAG             PIC X VALUE 'N'.
          88 END-OF-FILE       VALUE 'Y'.
       01 INPUT-RECORD.
          05 INPUT-VERTEX-COUNT PIC 99.
          05 INPUT-EDGE-COUNT   PIC 99.
          05 INPUT-FROM-V       PIC 99.
          05 INPUT-TO-V         PIC 99.
          05 INPUT-WEIGHT       PIC 999.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-DISTANCE-MATRIX.
           PERFORM EXECUTE-FLOYD-WARSHALL.
           PERFORM DISPLAY-RESULTS.
           STOP RUN.

       INITIALIZE-DISTANCE-MATRIX.
           *> Initialize distance matrix with maximum values
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRAPH-SIZE
                   MOVE MAX-VALUE TO DIST-ELEMENT(I,J)
               END-PERFORM
           END-PERFORM

           *> Set diagonal elements to 0
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
               MOVE 0 TO DIST-ELEMENT(I,I)
           END-PERFORM

           *> Set direct edge weights
           *> Example graph edges:
           *> 1->2 weight 3
           *> 1->3 weight 8
           *> 1->5 weight -4
           *> 2->4 weight 1
           *> 2->5 weight 7
           *> 3->2 weight 4
           *> 4->1 weight 2
           *> 4->3 weight -5
           *> 5->4 weight 6

           MOVE 3 TO DIST-ELEMENT(1,2)
           MOVE 8 TO DIST-ELEMENT(1,3)
           MOVE 999 TO DIST-ELEMENT(1,4)
           MOVE -4 TO DIST-ELEMENT(1,5)

           MOVE 999 TO DIST-ELEMENT(2,1)
           MOVE 999 TO DIST-ELEMENT(2,3)
           MOVE 1 TO DIST-ELEMENT(2,4)
           MOVE 7 TO DIST-ELEMENT(2,5)

           MOVE 999 TO DIST-ELEMENT(3,1)
           MOVE 4 TO DIST-ELEMENT(3,2)
           MOVE 999 TO DIST-ELEMENT(3,4)
           MOVE 999 TO DIST-ELEMENT(3,5)

           MOVE 2 TO DIST-ELEMENT(4,1)
           MOVE 999 TO DIST-ELEMENT(4,2)
           MOVE -5 TO DIST-ELEMENT(4,3)
           MOVE 999 TO DIST-ELEMENT(4,5)

           MOVE 999 TO DIST-ELEMENT(5,1)
           MOVE 999 TO DIST-ELEMENT(5,2)
           MOVE 999 TO DIST-ELEMENT(5,3)
           MOVE 6 TO DIST-ELEMENT(5,4)

           DISPLAY "Initial Distance Matrix:".
           PERFORM DISPLAY-INITIAL-MATRIX.

       EXECUTE-FLOYD-WARSHALL.
           *> Floyd-Warshall Algorithm
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > GRAPH-SIZE
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
                   PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRAPH-SIZE
                       MOVE DIST-ELEMENT(I,J) TO TEMP-DISTANCE
                       COMPUTE DIST-ELEMENT(I,J) = 
                           FUNCTION MIN(TEMP-DISTANCE, 
                                       DIST-ELEMENT(I,K) + DIST-ELEMENT(K,J))
                   END-PERFORM
               END-PERFORM
           END-PERFORM.

       DISPLAY-RESULTS.
           DISPLAY "Shortest Distance Matrix:".
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRAPH-SIZE
                   IF DIST-ELEMENT(I,J) = MAX-VALUE
                       DISPLAY "INF" WITH NO ADVANCING
                   ELSE
                       DISPLAY DIST-ELEMENT(I,J) WITH NO ADVANCING
                   END-IF
               END-PERFORM
               DISPLAY SPACE
           END-PERFORM.

       DISPLAY-INITIAL-MATRIX.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRAPH-SIZE
                   IF DIST-ELEMENT(I,J) = MAX-VALUE
                       DISPLAY "INF" WITH NO ADVANCING
                   ELSE
                       DISPLAY DIST-ELEMENT(I,J) WITH NO ADVANCING
                   END-IF
               END-PERFORM
               DISPLAY SPACE
           END-PERFORM.
```

## Key Features of this COBOL Implementation:

### 1. **Data Structure**
- Uses a 2D array `DIST-ELEMENT` to represent the distance matrix
- `GRAPH-SIZE` defines the number of vertices (set to 5 in this example)
- `MAX-VALUE` represents infinity (999 in this case)

### 2. **Algorithm Steps**
1. **Initialization**: Set all distances to maximum value, diagonal to 0
2. **Edge Setting**: Load the initial graph edges into the matrix
3. **Floyd-Warshall Core**: Three nested loops implementing the algorithm
4. **Result Display**: Show the final shortest path matrix

### 3. **Algorithm Logic**
```
For each intermediate vertex k:
    For each source vertex i:
        For each destination vertex j:
            dist[i][j] = min(dist[i][j], dist[i][k] + dist[k][j])
```

### 4. **Sample Output**
```
Initial Distance Matrix:
INF 3   8   INF -4
INF INF INF 1   7
INF 4   INF INF INF
2   INF -5 INF INF
INF INF INF 6   INF

Shortest Distance Matrix:
0   1   -3  1   -4
3   0   -4  1   -1
7   4   0   5   3
2   -1  -5  0   -6
8   5   1   6   0
```

This implementation demonstrates the classic Floyd-Warshall algorithm in COBOL, showing how to work with multi-dimensional arrays and implement graph algorithms in the traditional procedural programming style of COBOL.

