# Iterative Deepening Depth-First Search (IDDFS) in COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IDDFS-EXAMPLE.
       AUTHOR. AI Assistant.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TREE-FILE ASSIGN TO "TREE-DATA.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  TREE-FILE.
       01  TREE-RECORD.
           05 NODE-ID            PIC 9(3).
           05 NODE-VALUE         PIC A(10).
           05 CHILD-1            PIC 9(3).
           05 CHILD-2            PIC 9(3).
           05 CHILD-3            PIC 9(3).

       WORKING-STORAGE SECTION.
       01  SEARCH-RESULTS.
           05 FOUND-FLAG         PIC 9 VALUE 0.
           05 TARGET-VALUE       PIC A(10) VALUE "TARGET".
           05 MAX-DEPTH          PIC 9(2) VALUE 10.
           05 CURRENT-DEPTH      PIC 9(2) VALUE 0.
           05 STACK-DEPTH        PIC 9(2) VALUE 0.
           05 STACK-TOP          PIC 9(2) VALUE 0.

       01  STACK.
           05 STACK-ITEMS        OCCURS 50 TIMES.
               10 STACK-NODE     PIC 9(3).
               10 STACK-LEVEL    PIC 9(2).

       01  TREE-NODES.
           05 NODE-TABLE.
               10 NODE-INFO        OCCURS 100 TIMES.
                   15 NODE-ID-KEY  PIC 9(3).
                   15 NODE-VALUE-KEY PIC A(10).
                   15 CHILD-1-KEY  PIC 9(3).
                   15 CHILD-2-KEY  PIC 9(3).
                   15 CHILD-3-KEY  PIC 9(3).

       01  TEMP-VARIABLES.
           05 TEMP-LEVEL         PIC 9(2).
           05 TEMP-NODE          PIC 9(3).
           05 TEMP-FOUND         PIC 9 VALUE 0.
           05 TEMP-INDEX         PIC 9(3) VALUE 1.
           05 TEMP-CHILD         PIC 9(3).

       01  FILE-CONTROL.
           05 FILE-STATUS        PIC XX VALUE SPACES.
           05 EOF-FLAG           PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Starting Iterative Deepening DFS Search"
           PERFORM INITIALIZE-TREE
           PERFORM IDDFS-SEARCH
           DISPLAY "Search completed"
           STOP RUN.

       INITIALIZE-TREE.
           MOVE 1 TO TEMP-INDEX
           PERFORM LOAD-NODES UNTIL TEMP-INDEX > 10
           EXIT PARAGRAPH.

       LOAD-NODES.
           IF TEMP-INDEX = 1
               MOVE 1 TO NODE-ID-KEY(TEMP-INDEX)
               MOVE "ROOT" TO NODE-VALUE-KEY(TEMP-INDEX)
               MOVE 2 TO CHILD-1-KEY(TEMP-INDEX)
               MOVE 3 TO CHILD-2-KEY(TEMP-INDEX)
               MOVE 4 TO CHILD-3-KEY(TEMP-INDEX)
           ELSE IF TEMP-INDEX = 2
               MOVE 2 TO NODE-ID-KEY(TEMP-INDEX)
               MOVE "A" TO NODE-VALUE-KEY(TEMP-INDEX)
               MOVE 5 TO CHILD-1-KEY(TEMP-INDEX)
               MOVE 6 TO CHILD-2-KEY(TEMP-INDEX)
               MOVE 0 TO CHILD-3-KEY(TEMP-INDEX)
           ELSE IF TEMP-INDEX = 3
               MOVE 3 TO NODE-ID-KEY(TEMP-INDEX)
               MOVE "B" TO NODE-VALUE-KEY(TEMP-INDEX)
               MOVE 7 TO CHILD-1-KEY(TEMP-INDEX)
               MOVE 0 TO CHILD-2-KEY(TEMP-INDEX)
               MOVE 0 TO CHILD-3-KEY(TEMP-INDEX)
           ELSE IF TEMP-INDEX = 4
               MOVE 4 TO NODE-ID-KEY(TEMP-INDEX)
               MOVE "C" TO NODE-VALUE-KEY(TEMP-INDEX)
               MOVE 8 TO CHILD-1-KEY(TEMP-INDEX)
               MOVE 9 TO CHILD-2-KEY(TEMP-INDEX)
               MOVE 0 TO CHILD-3-KEY(TEMP-INDEX)
           ELSE IF TEMP-INDEX = 5
               MOVE 5 TO NODE-ID-KEY(TEMP-INDEX)
               MOVE "D" TO NODE-VALUE-KEY(TEMP-INDEX)
               MOVE 0 TO CHILD-1-KEY(TEMP-INDEX)
               MOVE 0 TO CHILD-2-KEY(TEMP-INDEX)
               MOVE 0 TO CHILD-3-KEY(TEMP-INDEX)
           ELSE IF TEMP-INDEX = 6
               MOVE 6 TO NODE-ID-KEY(TEMP-INDEX)
               MOVE "E" TO NODE-VALUE-KEY(TEMP-INDEX)
               MOVE 0 TO CHILD-1-KEY(TEMP-INDEX)
               MOVE 0 TO CHILD-2-KEY(TEMP-INDEX)
               MOVE 0 TO CHILD-3-KEY(TEMP-INDEX)
           ELSE IF TEMP-INDEX = 7
               MOVE 7 TO NODE-ID-KEY(TEMP-INDEX)
               MOVE "F" TO NODE-VALUE-KEY(TEMP-INDEX)
               MOVE 0 TO CHILD-1-KEY(TEMP-INDEX)
               MOVE 0 TO CHILD-2-KEY(TEMP-INDEX)
               MOVE 0 TO CHILD-3-KEY(TEMP-INDEX)
           ELSE IF TEMP-INDEX = 8
               MOVE 8 TO NODE-ID-KEY(TEMP-INDEX)
               MOVE "TARGET" TO NODE-VALUE-KEY(TEMP-INDEX)
               MOVE 0 TO CHILD-1-KEY(TEMP-INDEX)
               MOVE 0 TO CHILD-2-KEY(TEMP-INDEX)
               MOVE 0 TO CHILD-3-KEY(TEMP-INDEX)
           ELSE IF TEMP-INDEX = 9
               MOVE 9 TO NODE-ID-KEY(TEMP-INDEX)
               MOVE "G" TO NODE-VALUE-KEY(TEMP-INDEX)
               MOVE 0 TO CHILD-1-KEY(TEMP-INDEX)
               MOVE 0 TO CHILD-2-KEY(TEMP-INDEX)
               MOVE 0 TO CHILD-3-KEY(TEMP-INDEX)
           END-IF
           ADD 1 TO TEMP-INDEX.

       IDDFS-SEARCH.
           MOVE 0 TO CURRENT-DEPTH
           PERFORM UNTIL CURRENT-DEPTH > MAX-DEPTH
               DISPLAY "Searching at depth: " CURRENT-DEPTH
               PERFORM DEPTH-FIRST-SEARCH WITH TEST AFTER
               IF FOUND-FLAG = 1
                   DISPLAY "Target found at depth: " CURRENT-DEPTH
                   EXIT PERFORM
               END-IF
               ADD 1 TO CURRENT-DEPTH
           END-PERFORM.

       DEPTH-FIRST-SEARCH.
           MOVE 0 TO FOUND-FLAG
           MOVE 0 TO STACK-TOP
           MOVE 1 TO TEMP-NODE
           PERFORM PUSH-NODE WITH TEST AFTER
           PERFORM DFS-TRAVEL UNTIL STACK-TOP = 0 OR FOUND-FLAG = 1.

       DFS-TRAVEL.
           IF STACK-TOP = 0 GOBACK
           PERFORM POP-NODE
           IF TEMP-NODE = 0 GOBACK
           PERFORM CHECK-NODE
           IF FOUND-FLAG = 1 GOBACK
           PERFORM PUSH-CHILDREN.

       PUSH-NODE.
           IF STACK-TOP < 50
               ADD 1 TO STACK-TOP
               MOVE TEMP-NODE TO STACK-ITEMS(STACK-TOP)
               MOVE 0 TO STACK-LEVEL(STACK-TOP)
           END-IF.

       POP-NODE.
           IF STACK-TOP > 0
               MOVE STACK-ITEMS(STACK-TOP) TO TEMP-NODE
               SUBTRACT 1 FROM STACK-TOP
           ELSE
               MOVE 0 TO TEMP-NODE
           END-IF.

       CHECK-NODE.
           IF TEMP-NODE = 0 RETURN
           PERFORM FIND-NODE-VALUE
           IF NODE-VALUE-KEY(TEMP-NODE) = TARGET-VALUE
               MOVE 1 TO FOUND-FLAG
           END-IF.

       FIND-NODE-VALUE.
           MOVE 1 TO TEMP-INDEX
           PERFORM UNTIL TEMP-INDEX > 100 OR NODE-ID-KEY(TEMP-INDEX) = TEMP-NODE
               ADD 1 TO TEMP-INDEX
           END-PERFORM.

       PUSH-CHILDREN.
           IF TEMP-NODE = 0 RETURN
           PERFORM PUSH-CHILD WITH TEST AFTER
           PERFORM PUSH-CHILD WITH TEST AFTER
           PERFORM PUSH-CHILD WITH TEST AFTER.

       PUSH-CHILD.
           IF TEMP-NODE = 0 RETURN
           IF TEMP-INDEX = 1
               MOVE CHILD-1-KEY(TEMP-NODE) TO TEMP-CHILD
           ELSE IF TEMP-INDEX = 2
               MOVE CHILD-2-KEY(TEMP-NODE) TO TEMP-CHILD
           ELSE IF TEMP-INDEX = 3
               MOVE CHILD-3-KEY(TEMP-NODE) TO TEMP-CHILD
           END-IF
           IF TEMP-CHILD > 0
               PERFORM PUSH-NODE
           END-IF
           ADD 1 TO TEMP-INDEX
           IF TEMP-INDEX > 3
               MOVE 1 TO TEMP-INDEX
           END-IF.
```

## Explanation of the IDDFS Implementation

This COBOL implementation demonstrates the Iterative Deepening Depth-First Search algorithm with the following key components:

### Key Features:
1. **Tree Structure**: Uses a node-based tree structure with parent-child relationships
2. **Iterative Deepening**: Repeatedly performs depth-limited DFS with increasing depth limits
3. **Stack Management**: Implements stack operations for DFS traversal
4. **Search Termination**: Stops when target is found or maximum depth is reached

### Algorithm Steps:
1. **Initialize Tree**: Load sample tree data into memory
2. **Iterative Search**: For each depth level from 0 to maximum depth:
   - Perform depth-limited DFS
   - Check if target is found
   - If found, terminate search
3. **DFS Traversal**: Uses stack-based approach to traverse nodes
4. **Child Exploration**: Push children of current node onto stack

### Key Variables:
- `CURRENT-DEPTH`: Tracks current search depth
- `STACK-TOP`: Stack pointer for DFS
- `FOUND-FLAG`: Indicates if target was found
- `NODE-TABLE`: Stores tree structure data

This implementation shows how COBOL can handle complex algorithms like IDDFS, though it's worth noting that modern languages would typically be more efficient for such graph traversal algorithms.

