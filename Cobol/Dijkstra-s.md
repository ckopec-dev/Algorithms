# Dijkstra's Algorithm in COBOL

Here's an example implementation of Dijkstra's shortest path algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DIJKSTRA-ALGORITHM.
       AUTHOR. AI-ASSISTANT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 GRAPH-DATA.
          05 VERTICES           PIC 9(2) VALUE 6.
          05 MAX-EDGES          PIC 9(3) VALUE 10.
          05 DISTANCE.
             10 DIST-VALUE      PIC 9(3) OCCURS 6 TIMES.
          05 PREDECESSOR.
             10 PREV-VALUE      PIC 9(2) OCCURS 6 TIMES.
          05 VISITED.
             10 VISITED-FLAG    PIC 9(1) OCCURS 6 TIMES.
          05 ADJACENCY-MATRIX.
             10 MATRIX-ROW      PIC 9(3) OCCURS 6 TIMES.
                15 MATRIX-ELEMENT PIC 9(3) OCCURS 6 TIMES.

       01 TEMP-VARIABLES.
          05 CURRENT-VERTICE    PIC 9(2).
          05 MIN-DISTANCE       PIC 9(3).
          05 UNVISITED-COUNT    PIC 9(2).
          05 U                  PIC 9(2).
          05 V                  PIC 9(2).
          05 I                  PIC 9(2).
          05 J                  PIC 9(2).
          05 K                  PIC 9(2).
          05 MIN-INDEX          PIC 9(2).
          05 INFINITY           PIC 9(3) VALUE 999.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Dijkstra's Algorithm Implementation"
           DISPLAY "================================"

           PERFORM INITIALIZE-GRAPH
           PERFORM INITIALIZE-DISTANCES
           PERFORM DIJKSTRA-ALGORITHM
           PERFORM DISPLAY-RESULTS

           STOP RUN.

       INITIALIZE-GRAPH.
           DISPLAY "Initializing Graph..."

           MOVE 0 TO UNVISITED-COUNT

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > VERTICES
               MOVE 0 TO VISITED-FLAG(I)
               MOVE 0 TO PREV-VALUE(I)
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > VERTICES
                   MOVE 0 TO MATRIX-ROW(I)(J)
               END-PERFORM
           END-PERFORM

           *> Define edges of the graph
           *> Edge from vertex 1 to 2 with weight 4
           MOVE 4 TO MATRIX-ROW(1)(2)
           MOVE 4 TO MATRIX-ROW(2)(1)

           *> Edge from vertex 1 to 3 with weight 2
           MOVE 2 TO MATRIX-ROW(1)(3)
           MOVE 2 TO MATRIX-ROW(3)(1)

           *> Edge from vertex 2 to 3 with weight 1
           MOVE 1 TO MATRIX-ROW(2)(3)
           MOVE 1 TO MATRIX-ROW(3)(2)

           *> Edge from vertex 2 to 4 with weight 5
           MOVE 5 TO MATRIX-ROW(2)(4)
           MOVE 5 TO MATRIX-ROW(4)(2)

           *> Edge from vertex 3 to 4 with weight 8
           MOVE 8 TO MATRIX-ROW(3)(4)
           MOVE 8 TO MATRIX-ROW(4)(3)

           *> Edge from vertex 3 to 5 with weight 10
           MOVE 10 TO MATRIX-ROW(3)(5)
           MOVE 10 TO MATRIX-ROW(5)(3)

           *> Edge from vertex 4 to 5 with weight 2
           MOVE 2 TO MATRIX-ROW(4)(5)
           MOVE 2 TO MATRIX-ROW(5)(4)

           *> Edge from vertex 4 to 6 with weight 6
           MOVE 6 TO MATRIX-ROW(4)(6)
           MOVE 6 TO MATRIX-ROW(6)(4)

           *> Edge from vertex 5 to 6 with weight 3
           MOVE 3 TO MATRIX-ROW(5)(6)
           MOVE 3 TO MATRIX-ROW(6)(5)

           DISPLAY "Graph initialized with 6 vertices and edges"
           DISPLAY "Adjacency Matrix:"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > VERTICES
               DISPLAY "Row " I ": " 
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > VERTICES
                   DISPLAY MATRIX-ROW(I)(J) " "
               END-PERFORM
               DISPLAY SPACE
           END-PERFORM.

       INITIALIZE-DISTANCES.
           DISPLAY "Initializing distances..."

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > VERTICES
               MOVE INFINITY TO DIST-VALUE(I)
           END-PERFORM

           *> Distance from source vertex (vertex 1) to itself is 0
           MOVE 0 TO DIST-VALUE(1)

           DISPLAY "Distances initialized"
           DISPLAY "Source vertex: 1"
           DISPLAY "Initial distances: "
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > VERTICES
               DISPLAY "Vertex " I ": " DIST-VALUE(I)
           END-PERFORM.

       DIJKSTRA-ALGORITHM.
           DISPLAY "Running Dijkstra's Algorithm..."

           *> Set all vertices as unvisited
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > VERTICES
               MOVE 0 TO VISITED-FLAG(I)
           END-PERFORM

           *> Set unvisited count
           MOVE VERTICES TO UNVISITED-COUNT

           *> Main loop of Dijkstra's algorithm
           PERFORM UNTIL UNVISITED-COUNT = 0
               *> Find vertex with minimum distance that is not visited
               MOVE INFINITY TO MIN-DISTANCE
               MOVE 0 TO MIN-INDEX

               PERFORM VARYING I FROM 1 BY 1 UNTIL I > VERTICES
                   IF VISITED-FLAG(I) = 0
                       AND DIST-VALUE(I) < MIN-DISTANCE
                       MOVE DIST-VALUE(I) TO MIN-DISTANCE
                       MOVE I TO MIN-INDEX
                   END-IF
               END-PERFORM

               *> If no unvisited vertex found, break
               IF MIN-INDEX = 0 THEN
                   GO TO DIJKSTRA-END
               END-IF

               *> Mark the vertex as visited
               MOVE 1 TO VISITED-FLAG(MIN-INDEX)
               SUBTRACT 1 FROM UNVISITED-COUNT

               *> Update distances of adjacent vertices
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > VERTICES
                   IF MATRIX-ROW(MIN-INDEX)(J) > 0
                       AND VISITED-FLAG(J) = 0
                       *> If we found a shorter path to vertex J
                       COMPUTE U = DIST-VALUE(MIN-INDEX) + MATRIX-ROW(MIN-INDEX)(J)
                       IF U < DIST-VALUE(J)
                           MOVE U TO DIST-VALUE(J)
                           MOVE MIN-INDEX TO PREV-VALUE(J)
                       END-IF
                   END-IF
               END-PERFORM

           END-PERFORM.

       DIJKSTRA-END.
           DISPLAY "Dijkstra's Algorithm completed."

       DISPLAY-RESULTS.
           DISPLAY "Shortest paths from vertex 1:"
           DISPLAY "================================"
           DISPLAY "Vertex  Distance  Predecessor"

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > VERTICES
               DISPLAY I "        " DIST-VALUE(I) "        " PREV-VALUE(I)
           END-PERFORM

           DISPLAY "================================"
           DISPLAY "Path reconstruction example:"
           DISPLAY "Shortest path from vertex 1 to vertex 6:"
           PERFORM RECONSTRUCT-PATH 20.

       RECONSTRUCT-PATH.
           DISPLAY "Reconstructing path from vertex 6 to 1"
           MOVE 6 TO CURRENT-VERTICE
           DISPLAY "Path: " CURRENT-VERTICE

           *> Backtrack using predecessor array
           PERFORM UNTIL CURRENT-VERTICE = 1
               MOVE PREV-VALUE(CURRENT-VERTICE) TO CURRENT-VERTICE
               DISPLAY " -> " CURRENT-VERTICE
           END-PERFORM
           DISPLAY "End of path"

       END PROGRAM DIJKSTRA-ALGORITHM.
```

## Explanation of Key Components

### 1. **Data Division**
- **GRAPH-DATA**: Contains all necessary data structures
- **VERTICES**: Number of vertices in the graph (6)
- **DISTANCE**: Array to store shortest distances from source
- **PREDECESSOR**: Array to store the previous vertex in the shortest path
- **VISITED**: Boolean array to track visited vertices
- **ADJACENCY-MATRIX**: 2D array representing the graph edges and weights

### 2. **Algorithm Steps**
1. **Initialization**: Set up the graph with edges and weights
2. **Distance Initialization**: Set all distances to infinity except source vertex (0)
3. **Main Loop**: 
   - Find unvisited vertex with minimum distance
   - Mark it as visited
   - Update distances of adjacent vertices
4. **Result Display**: Show shortest paths from source to all vertices

### 3. **Key Features**
- Uses adjacency matrix representation
- Implements the standard Dijkstra's algorithm logic
- Includes path reconstruction functionality
- Handles multiple vertices and weighted edges

This implementation demonstrates how Dijkstra's algorithm can be structured in COBOL, though it's worth noting that modern programming languages are typically preferred for graph algorithms due to their more concise syntax and better data structure support.

