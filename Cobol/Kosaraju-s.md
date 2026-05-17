# Kosaraju's Algorithm in COBOL

Kosaraju's algorithm is used to find strongly connected components in a directed graph. Here's an implementation in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. KOSARAJU-ALGORITHM.
       AUTHOR. COBOL IMPLEMENTATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GRAPH-FILE ASSIGN TO "graph.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD GRAPH-FILE.
       01 GRAPH-RECORD.
          05 SOURCE-NODE    PIC 9(3).
          05 DEST-NODE      PIC 9(3).

       WORKING-STORAGE SECTION.
       01 GRAPH-SIZE        PIC 9(3) VALUE 6.
       01 STACK-SIZE        PIC 9(3) VALUE 0.
       01 COMPONENT-COUNT   PIC 9(3) VALUE 0.
       
       01 ADJACENCY-LIST.
          05 NODE-ITEM OCCURS 100 TIMES.
             10 ADJ-NODES OCCURS 20 TIMES PIC 9(3).
             10 ADJ-INDEX PIC 9(3) VALUE 0.
       
       01 REVERSE-LIST.
          05 REV-NODE-ITEM OCCURS 100 TIMES.
             10 REV-ADJ-NODES OCCURS 20 TIMES PIC 9(3).
             10 REV-INDEX PIC 9(3) VALUE 0.
       
       01 VISITED-ARRAY.
          05 VISITED OCCURS 100 TIMES PIC 9 VALUE 0.
       
       01 STACK-ARRAY.
          05 STACK OCCURS 100 TIMES PIC 9(3).
       
       01 COMPONENT-ARRAY.
          05 COMPONENT OCCURS 100 TIMES PIC 9(3).
       
       01 I, J, K, L, M, N PIC 9(3).
       01 TEMP-NODE PIC 9(3).
       01 RECORD-COUNT PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "KOSARAJU'S ALGORITHM IMPLEMENTATION".
           DISPLAY "------------------------------------".
           
           PERFORM INITIALIZE-GRAPH.
           PERFORM READ-GRAPH-DATA.
           PERFORM BUILD-ADJACENCY-LIST.
           PERFORM FIRST-DFS.
           PERFORM BUILD-REVERSE-LIST.
           PERFORM SECOND-DFS.
           PERFORM DISPLAY-RESULTS.
           
           STOP RUN.

       INITIALIZE-GRAPH.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 100
               MOVE 0 TO VISITED(I)
               MOVE 0 TO ADJ-INDEX(I)
               MOVE 0 TO REV-INDEX(I)
           END-PERFORM.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 100
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 20
                   MOVE 0 TO ADJ-NODES(I)(J)
                   MOVE 0 TO REV-ADJ-NODES(I)(J)
               END-PERFORM
           END-PERFORM.
           
           MOVE 0 TO STACK-SIZE.
           MOVE 0 TO COMPONENT-COUNT.

       READ-GRAPH-DATA.
           OPEN INPUT GRAPH-FILE.
           READ GRAPH-FILE AT END GO TO END-OF-FILE.
           
       PROCESS-RECORD.
           ADD 1 TO RECORD-COUNT.
           MOVE SOURCE-NODE TO TEMP-NODE.
           ADD 1 TO ADJ-INDEX(TEMP-NODE).
           MOVE DEST-NODE TO ADJ-NODES(TEMP-NODE)(ADJ-INDEX(TEMP-NODE)).
           READ GRAPH-FILE AT END GO TO END-OF-FILE.
           GO TO PROCESS-RECORD.
           
       END-OF-FILE.
           CLOSE GRAPH-FILE.

       BUILD-ADJACENCY-LIST.
           DISPLAY "Building adjacency list...".
           DISPLAY "Graph size: " GRAPH-SIZE.

       FIRST-DFS.
           DISPLAY "First DFS traversal...".
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
               IF VISITED(I) = 0
                   PERFORM DFS-RECURSIVE I
               END-IF
           END-PERFORM.

       DFS-RECURSIVE.
           ACCEPT I.
           MOVE 1 TO VISITED(I).
           DISPLAY "Visiting node: " I.
           
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > ADJ-INDEX(I)
               IF VISITED(ADJ-NODES(I)(J)) = 0
                   PERFORM DFS-RECURSIVE ADJ-NODES(I)(J)
               END-IF
           END-PERFORM.
           
           ADD 1 TO STACK-SIZE.
           MOVE I TO STACK(STACK-SIZE).

       BUILD-REVERSE-LIST.
           DISPLAY "Building reverse graph...".
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
               MOVE 0 TO VISITED(I)
           END-PERFORM.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRAPH-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > ADJ-INDEX(I)
                   ADD 1 TO REV-INDEX(ADJ-NODES(I)(J))
                   MOVE I TO REV-ADJ-NODES(ADJ-NODES(I)(J))(REV-INDEX(ADJ-NODES(I)(J)))
               END-PERFORM
           END-PERFORM.

       SECOND-DFS.
           DISPLAY "Second DFS on reverse graph...".
           PERFORM VARYING I FROM STACK-SIZE BY -1 UNTIL I < 1
               IF VISITED(STACK(I)) = 0
                   ADD 1 TO COMPONENT-COUNT.
                   PERFORM DFS-REVERSE-RECURSIVE STACK(I)
                   DISPLAY "Strongly Connected Component " COMPONENT-COUNT
               END-IF
           END-PERFORM.

       DFS-REVERSE-RECURSIVE.
           ACCEPT I.
           MOVE 1 TO VISITED(I).
           DISPLAY "  Node " I " belongs to component " COMPONENT-COUNT.
           
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > REV-INDEX(I)
               IF VISITED(REV-ADJ-NODES(I)(J)) = 0
                   PERFORM DFS-REVERSE-RECURSIVE REV-ADJ-NODES(I)(J)
               END-IF
           END-PERFORM.

       DISPLAY-RESULTS.
           DISPLAY "------------------------------------".
           DISPLAY "KOSARAJU'S ALGORITHM RESULTS".
           DISPLAY "------------------------------------".
           DISPLAY "Number of strongly connected components: " COMPONENT-COUNT.
           DISPLAY "------------------------------------".
           DISPLAY "Algorithm completed successfully.".
```

## Sample Input File (graph.dat)
```
1 2
2 3
3 1
3 4
4 5
5 6
6 4
```

## Key Features of This Implementation:

1. **Graph Representation**: Uses adjacency lists to store the graph
2. **Two-Phase DFS**: Implements the classic two-phase approach of Kosaraju's algorithm
3. **Stack Management**: Uses a stack to store nodes in finishing order
4. **Reverse Graph**: Builds a reverse graph for the second DFS
5. **Component Tracking**: Identifies and displays strongly connected components

## Algorithm Steps:
1. Build adjacency list representation of the graph
2. Perform first DFS to get finishing order (push nodes to stack)
3. Build reverse graph
4. Perform second DFS on reverse graph in stack order
5. Output strongly connected components

This implementation demonstrates the core concepts of Kosaraju's algorithm in COBOL syntax, though actual execution would require a COBOL compiler and proper input data files.

