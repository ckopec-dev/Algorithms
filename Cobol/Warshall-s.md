# Floyd-Warshall Algorithm in COBOL

Here's an implementation of the Floyd-Warshall algorithm for finding shortest paths between all pairs of vertices in a weighted graph using COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FLOYD-WARSHALL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  GRAPH-SIZE           PIC 9(2) VALUE 4.
       01  INF                  PIC 9(5) VALUE 99999.
       01  I                    PIC 9(2).
       01  J                    PIC 9(2).
       01  K                    PIC 9(2).
       01  TEMP                 PIC 9(5).

       01  DISTANCE-MATRIX.
           05  DIST OCCURS 10 TIMES INDEXED BY DI.
               10  ROW OCCURS 10 TIMES INDEXED BY DJ.
                   15  DIST-VALUE        PIC 9(5).

       01  PATH-MATRIX.
           05  PATH OCCURS 10 TIMES INDEXED BY PI.
               10  PATH-ROW OCCURS 10 TIMES INDEXED BY PJ.
                   15  PATH-VALUE        PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM INITIALIZE-MATRIX.
           PERFORM FLOYD-WARSHALL-ALGORITHM.
           PERFORM DISPLAY-RESULTS.
           STOP RUN.

       INITIALIZE-MATRIX.
           MOVE 0 TO I, J, K

           * Initialize distance matrix with input values
           MOVE 0 TO DIST-VALUE(1,1)
           MOVE 0 TO DIST-VALUE(2,2)
           MOVE 0 TO DIST-VALUE(3,3)
           MOVE 0 TO DIST-VALUE(4,4)

           MOVE 3 TO DIST-VALUE(1,2)
           MOVE 8 TO DIST-VALUE(1,3)
           MOVE 4 TO DIST-VALUE(1,4)
           MOVE 5 TO DIST-VALUE(2,1)
           MOVE 0 TO DIST-VALUE(2,3)
           MOVE 9 TO DIST-VALUE(2,4)
           MOVE 2 TO DIST-VALUE(3,1)
           MOVE 6 TO DIST-VALUE(3,2)
           MOVE 0 TO DIST-VALUE(3,4)
           MOVE 7 TO DIST-VALUE(4,1)
           MOVE 1 TO DIST-VALUE(4,2)
           MOVE 8 TO DIST-VALUE(4,3)

           * Set infinity for direct connections
           PERFORM SET-INFINITY-VIA-INDEX

           * Initialize path matrix with direct paths
           PERFORM INITIALIZE-PATH-MATRIX

           GOBACK.

       SET-INFINITY-VIA-INDEX.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRAPH-SIZE
                   IF DIST-VALUE(I,J) = 0 AND I NOT = J
                       MOVE INF TO DIST-VALUE(I,J)
                   END-IF
               END-PERFORM
           END-PERFORM.

       INITIALIZE-PATH-MATRIX.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRAPH-SIZE
                   IF DIST-VALUE(I,J) < INF AND I NOT = J
                       MOVE J TO PATH-VALUE(I,J)
                   ELSE
                       MOVE 0 TO PATH-VALUE(I,J)
                   END-IF
               END-PERFORM
           END-PERFORM.

       FLOYD-WARSHALL-ALGORITHM.
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > GRAPH-SIZE
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
                   PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRAPH-SIZE
                       COMPUTE TEMP = DIST-VALUE(I,K) + DIST-VALUE(K,J)
                       IF TEMP < DIST-VALUE(I,J)
                           MOVE TEMP TO DIST-VALUE(I,J)
                           MOVE K TO PATH-VALUE(I,J)
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-PERFORM.

       DISPLAY-RESULTS.
           DISPLAY "SHORTEST DISTANCE MATRIX:"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRAPH-SIZE
                   IF DIST-VALUE(I,J) = INF
                       DISPLAY "INF"
                   ELSE
                       DISPLAY DIST-VALUE(I,J)
                   END-IF
               END-PERFORM
               DISPLAY " "
           END-PERFORM.

           DISPLAY "PATH MATRIX:"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRAPH-SIZE
                   DISPLAY PATH-VALUE(I,J)
               END-PERFORM
               DISPLAY " "
           END-PERFORM.
```

## Algorithm Explanation

This COBOL implementation of Floyd-Warshall algorithm:

1. **Initialization**: Sets up a 4x4 distance matrix with sample weights between vertices
2. **Distance Matrix**: Stores the shortest distances between all pairs of vertices
3. **Path Matrix**: Tracks intermediate vertices to reconstruct actual paths
4. **Main Algorithm**: Uses three nested loops to consider each vertex as an intermediate point
5. **Optimization**: Updates distances if a shorter path is found through intermediate vertices

## Key Features

- **Three-level nested loop structure** for the core algorithm
- **Matrix-based representation** of graph data
- **Infinity handling** for unconnected vertices
- **Path reconstruction** capability
- **Proper COBOL syntax** with indexed arrays and structured programming

The algorithm has O(n³) time complexity where n is the number of vertices in the graph.