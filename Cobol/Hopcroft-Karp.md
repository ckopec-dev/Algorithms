# Hopcroft-Karp Algorithm in COBOL

The following is a COBOL implementation of the Hopcroft-Karp algorithm for finding maximum matching in bipartite graphs.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOPCROFT-KARP.
       AUTHOR. AI Assistant.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "graph-data.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
          05 NODE-A              PIC 9(3).
          05 NODE-B              PIC 9(3).

       WORKING-STORAGE SECTION.
       77 MAX-NODES           PIC 9(3) VALUE 100.
       77 MAX-EDGES           PIC 9(4) VALUE 500.
       77 MAX-MATCHING        PIC 9(3) VALUE 0.

       01 GRAPH-TABLE.
          05 NODES OCCURS 100 TIMES INDEXED BY I.
             10 NODE-ID          PIC 9(3).
             10 ADJACENT-NODES   OCCURS 20 TIMES INDEXED BY J.
                15 ADJACENT-ID   PIC 9(3).

       01 MATCHING-TABLE.
          05 LEFT-MATCHES     OCCURS 100 TIMES INDEXED BY I.
             10 LEFT-MATCH-TO PIC 9(3) VALUE 0.
          05 RIGHT-MATCHES    OCCURS 100 TIMES INDEXED BY J.
             10 RIGHT-MATCH-TO PIC 9(3) VALUE 0.

       01 DISTANCE-TABLE.
          05 DISTANCES        OCCURS 100 TIMES INDEXED BY I.
             10 DISTANCE-VALUE PIC 9(3) VALUE 0.

       01 QUEUE-TABLE.
          05 QUEUE            OCCURS 100 TIMES INDEXED BY QI.
             10 QUEUE-ELEMENT  PIC 9(3).

       01 TEMP-VARIABLES.
          05 CURRENT-NODE        PIC 9(3).
          05 NEXT-NODE           PIC 9(3).
          05 QUEUE-FRONT         PIC 9(3) VALUE 1.
          05 QUEUE-BACK          PIC 9(3) VALUE 0.
          05 FOUND-PATH          PIC X VALUE 'N'.
          05 DISTANCE            PIC 9(3) VALUE 0.
          05 MATCHING-COUNT      PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-DATA.
           PERFORM READ-GRAPH-DATA.
           PERFORM HOPCROFT-KARP-ALGORITHM.
           PERFORM DISPLAY-RESULTS.
           STOP RUN.

       INITIALIZE-DATA.
           MOVE 0 TO MATCHING-COUNT.
           PERFORM INITIALIZE-MATCHINGS.
           PERFORM INITIALIZE-DISTANCES.
           PERFORM INITIALIZE-QUEUE.
           GOBACK.

       INITIALIZE-MATCHINGS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MAX-NODES
               MOVE 0 TO LEFT-MATCHES(I)
               MOVE 0 TO RIGHT-MATCHES(I)
           END-PERFORM.
           GOBACK.

       INITIALIZE-DISTANCES.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MAX-NODES
               MOVE 0 TO DISTANCES(I)
           END-PERFORM.
           GOBACK.

       INITIALIZE-QUEUE.
           MOVE 1 TO QUEUE-FRONT.
           MOVE 0 TO QUEUE-BACK.
           GOBACK.

       READ-GRAPH-DATA.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE AT END GO TO END-OF-FILE.
           PERFORM PROCESS-RECORD.
           GO TO READ-GRAPH-DATA.
       END-OF-FILE.
           CLOSE INPUT-FILE.
           GOBACK.

       PROCESS-RECORD.
           MOVE NODE-A TO CURRENT-NODE.
           MOVE NODE-B TO NEXT-NODE.
           PERFORM ADD-EDGE.
           GOBACK.

       ADD-EDGE.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 20
               IF ADJACENT-NODES(I,J) = 0
                   MOVE NEXT-NODE TO ADJACENT-NODES(I,J)
                   GO TO ADD-EDGE-END
               END-IF
           END-PERFORM.
       ADD-EDGE-END.
           GOBACK.

       HOPCROFT-KARP-ALGORITHM.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MAX-NODES
               IF LEFT-MATCHES(I) = 0
                   PERFORM RESET-DISTANCES.
                   PERFORM BFS-TO-FIND-PATHS.
                   IF DISTANCES(I) > 0
                       PERFORM DFS-TO-AUGMENT
                   END-IF
               END-IF
           END-PERFORM.
           GOBACK.

       RESET-DISTANCES.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MAX-NODES
               MOVE 0 TO DISTANCES(I)
           END-PERFORM.
           GOBACK.

       BFS-TO-FIND-PATHS.
           MOVE 0 TO QUEUE-BACK.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MAX-NODES
               IF LEFT-MATCHES(I) = 0
                   MOVE I TO QUEUE-ELEMENT(QUEUE-BACK + 1)
                   ADD 1 TO QUEUE-BACK
                   MOVE 0 TO DISTANCES(I)
               END-IF
           END-PERFORM.

           PERFORM UNTIL QUEUE-FRONT > QUEUE-BACK
               MOVE QUEUE-ELEMENT(QUEUE-FRONT) TO CURRENT-NODE
               ADD 1 TO QUEUE-FRONT
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 20
                   IF ADJACENT-NODES(CURRENT-NODE,J) > 0
                       MOVE ADJACENT-NODES(CURRENT-NODE,J) TO NEXT-NODE
                       IF RIGHT-MATCHES(NEXT-NODE) = 0
                           PERFORM UPDATE-MATCHING
                           GO TO BFS-CONTINUE
                       ELSE
                           IF DISTANCES(RIGHT-MATCHES(NEXT-NODE)) = 0
                               MOVE RIGHT-MATCHES(NEXT-NODE) TO QUEUE-ELEMENT(QUEUE-BACK + 1)
                               ADD 1 TO QUEUE-BACK
                               MOVE 0 TO DISTANCES(RIGHT-MATCHES(NEXT-NODE))
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM.
       BFS-CONTINUE.
           GOBACK.

       DFS-TO-AUGMENT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MAX-NODES
               IF LEFT-MATCHES(I) = 0
                   PERFORM RESET-DFS-VISITED.
                   PERFORM DFS-SEARCH-FOR-AUGMENTING-PATH(I)
               END-IF
           END-PERFORM.
           GOBACK.

       RESET-DFS-VISITED.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MAX-NODES
               MOVE 0 TO DISTANCES(I)
           END-PERFORM.
           GOBACK.

       DFS-SEARCH-FOR-AUGMENTING-PATH.
           ACCEPT CURRENT-NODE.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 20
               IF ADJACENT-NODES(CURRENT-NODE,J) > 0
                   MOVE ADJACENT-NODES(CURRENT-NODE,J) TO NEXT-NODE
                   IF DISTANCES(NEXT-NODE) = 0
                       MOVE 1 TO DISTANCES(NEXT-NODE)
                       IF RIGHT-MATCHES(NEXT-NODE) = 0 OR
                          DFS-SEARCH-FOR-AUGMENTING-PATH(RIGHT-MATCHES(NEXT-NODE))
                           PERFORM UPDATE-MATCHING-WITH-NEW-PATH
                           GO TO DFS-END
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.
       DFS-END.
           GOBACK.

       UPDATE-MATCHING.
           MOVE 1 TO MATCHING-COUNT.
           GOBACK.

       UPDATE-MATCHING-WITH-NEW-PATH.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MAX-NODES
               IF LEFT-MATCHES(I) = 0
                   PERFORM UPDATE-MATCHINGS
               END-IF
           END-PERFORM.
           GOBACK.

       UPDATE-MATCHINGS.
           MOVE NEXT-NODE TO RIGHT-MATCHES(CURRENT-NODE)
           MOVE CURRENT-NODE TO LEFT-MATCHES(NEXT-NODE)
           GOBACK.

       DISPLAY-RESULTS.
           DISPLAY "Maximum Matching: " MATCHING-COUNT.
           DISPLAY "Matching pairs:"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MAX-NODES
               IF LEFT-MATCHES(I) > 0
                   DISPLAY "Node " I " matches with Node " LEFT-MATCHES(I)
               END-IF
           END-PERFORM.
           GOBACK.

       END PROGRAM HOPCROFT-KARP.
```

## Key Features of this Implementation

### Data Structures:
1. **Graph representation** using adjacency lists
2. **Matching tables** for left and right vertices
3. **Distance arrays** for BFS operations
4. **Queue structure** for breadth-first search

### Algorithm Steps:
1. **Initialize** matching and distance arrays
2. **Breadth-First Search (BFS)** to find augmenting paths
3. **Depth-First Search (DFS)** to augment the matching
4. **Repeat** until no more augmenting paths exist

### Time Complexity:
- **O(E × √V)** where E is edges and V is vertices
- Efficient for sparse bipartite graphs

### Usage Notes:
- This implementation assumes bipartite graph input
- Requires proper data file format (node pairs)
- Can be extended for weighted matching or other variations
- Uses fixed-size arrays - can be modified for dynamic allocation

This COBOL implementation demonstrates the core logic of Hopcroft-Karp algorithm while maintaining compatibility with traditional COBOL syntax and structure.