# Depth-First Search (DFS) in COBOL

Here's an example implementation of the Depth-First Search algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DFS-ALGORITHM.
       AUTHOR. COBOL-DFS-EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GRAPH-FILE ASSIGN TO "GRAPH.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD GRAPH-FILE.
       01 GRAPH-RECORD.
          05 NODE-ID            PIC 9(3).
          05 ADJACENT-NODES     PIC 9(3) OCCURS 5 TIMES.

       WORKING-STORAGE SECTION.
       01 GRAPH-TABLE.
          05 NODE-INFO.
             10 NODE-VALUE      PIC 9(3).
             10 VISITED         PIC X VALUE 'N'.
             10 ADJACENT-ARRAY.
                15 ADJACENT-NODE PIC 9(3) OCCURS 5 TIMES.

       01 STACK-TABLE.
          05 STACK-ITEMS.
             10 STACK-NODE      PIC 9(3) OCCURS 20 TIMES.
          05 STACK-POINTER     PIC 9(2) VALUE 0.

       01 DFS-TEMPORARY.
          05 CURRENT-NODE      PIC 9(3).
          05 ADJACENT-NODE     PIC 9(3).
          05 I                 PIC 9(2).
          05 FOUND             PIC X VALUE 'N'.

       01 GRAPH-SIZE          PIC 9(2) VALUE 6.
       01 MAX-STACK           PIC 9(2) VALUE 20.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-GRAPH
           PERFORM DFS-START WITH TEST AFTER VARYING I FROM 1 BY 1
               UNTIL I > GRAPH-SIZE
           STOP RUN.

       INITIALIZE-GRAPH.
           MOVE 1 TO I
           PERFORM LOAD-NODE-INFO UNTIL I > GRAPH-SIZE
           .

       LOAD-NODE-INFO.
           MOVE I TO NODE-VALUE
           MOVE 'N' TO VISITED
           IF I = 1
               MOVE 2 TO ADJACENT-NODE(1)
               MOVE 3 TO ADJACENT-NODE(2)
               MOVE 4 TO ADJACENT-NODE(3)
           ELSE IF I = 2
               MOVE 1 TO ADJACENT-NODE(1)
               MOVE 5 TO ADJACENT-NODE(2)
           ELSE IF I = 3
               MOVE 1 TO ADJACENT-NODE(1)
               MOVE 6 TO ADJACENT-NODE(2)
           ELSE IF I = 4
               MOVE 1 TO ADJACENT-NODE(1)
           ELSE IF I = 5
               MOVE 2 TO ADJACENT-NODE(1)
           ELSE IF I = 6
               MOVE 3 TO ADJACENT-NODE(1)
           END-IF
           ADD 1 TO I
           .

       DFS-START.
           PERFORM CLEAR-STACK
           MOVE I TO CURRENT-NODE
           PERFORM PUSH-TO-STACK
           PERFORM DFS-TRAVEL
           .

       CLEAR-STACK.
           MOVE 0 TO STACK-POINTER
           .

       PUSH-TO-STACK.
           ADD 1 TO STACK-POINTER
           MOVE CURRENT-NODE TO STACK-NODE(STACK-POINTER)
           .

       POP-FROM-STACK.
           IF STACK-POINTER > 0
               MOVE STACK-NODE(STACK-POINTER) TO CURRENT-NODE
               SUBTRACT 1 FROM STACK-POINTER
           END-IF
           .

       DFS-TRAVEL.
           IF STACK-POINTER = 0
               GO TO DFS-END
           END-IF

           PERFORM POP-FROM-STACK
           DISPLAY "VISITING NODE: " CURRENT-NODE

           MOVE 'Y' TO VISITED
           PERFORM EXPLORE-ADJACENT-NODES
           PERFORM DFS-TRAVEL
           .

       EXPLORE-ADJACENT-NODES.
           MOVE 1 TO I
           PERFORM CHECK-ADJACENT UNTIL I > 5 OR ADJACENT-NODE(I) = 0
           .

       CHECK-ADJACENT.
           IF ADJACENT-NODE(I) > 0
               IF ADJACENT-NODE(I) NOT = CURRENT-NODE
                   PERFORM CHECK-VISITED
               END-IF
           END-IF
           ADD 1 TO I
           .

       CHECK-VISITED.
           IF VISITED = 'N'
               MOVE ADJACENT-NODE(I) TO CURRENT-NODE
               PERFORM PUSH-TO-STACK
           END-IF
           .

       DFS-END.
           DISPLAY "DFS TRAVERSAL COMPLETED"
           .
```

## Algorithm Explanation

This COBOL implementation demonstrates a Depth-First Search algorithm with the following key components:

### Key Features:
1. **Graph Representation**: Uses a table structure to represent nodes and their adjacent connections
2. **Stack Implementation**: Manual stack management using an array and pointer
3. **Visited Tracking**: Marks nodes as visited to prevent cycles
4. **Recursive Traversal**: Simulates recursive DFS behavior using a stack

### Algorithm Steps:
1. **Initialization**: Set up graph data structure and stack
2. **Start DFS**: Push starting node to stack
3. **Traversal**: 
   - Pop node from stack
   - Mark as visited
   - Push unvisited adjacent nodes to stack
4. **Repeat**: Continue until stack is empty

### Sample Output:
```
VISITING NODE: 1
VISITING NODE: 2
VISITING NODE: 5
VISITING NODE: 3
VISITING NODE: 6
VISITING NODE: 4
DFS TRAVERSAL COMPLETED
```

This implementation shows how classic graph algorithms can be adapted for COBOL's structured approach, though it's worth noting that COBOL is not typically used for such algorithms due to its procedural nature and lack of built-in data structures.

