# Binary Search Tree in COBOL

Here's an example implementation of a Binary Search Tree algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BINARY-SEARCH-TREE.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TREE-FILE ASSIGN TO "TREE.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  TREE-FILE.
       01  TREE-RECORD.
           05  NODE-VALUE        PIC 9(5).
           05  LEFT-CHILD        PIC 9(5).
           05  RIGHT-CHILD       PIC 9(5).

       WORKING-STORAGE SECTION.
       01  TREE-NODE.
           05  NODE-KEY          PIC 9(5).
           05  NODE-LEFT         PIC 9(5) VALUE ZERO.
           05  NODE-RIGHT        PIC 9(5) VALUE ZERO.
           05  NODE-LEVEL        PIC 9(2) VALUE ZERO.

       01  ROOT-NODE             PIC 9(5) VALUE ZERO.
       01  CURRENT-NODE          PIC 9(5) VALUE ZERO.
       01  SEARCH-KEY            PIC 9(5).
       01  FOUND-FLAG            PIC X VALUE 'N'.
           88  NODE-FOUND        VALUE 'Y'.
           88  NODE-NOT-FOUND    VALUE 'N'.
       01  TEMP-NODE             PIC 9(5) VALUE ZERO.
       01  TEMP-LEFT             PIC 9(5) VALUE ZERO.
       01  TEMP-RIGHT            PIC 9(5) VALUE ZERO.

       01  TREE-STATUS           PIC X VALUE 'O'.
           88  TREE-EMPTY        VALUE 'E'.
           88  TREE-NON-EMPTY    VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "BINARY SEARCH TREE EXAMPLE"
           DISPLAY "==========================="

           PERFORM INITIALIZE-TREE
           PERFORM INSERT-NODES
           PERFORM SEARCH-NODES
           PERFORM DISPLAY-TREE
           STOP RUN.

       INITIALIZE-TREE.
           MOVE ZERO TO ROOT-NODE
           MOVE 'E' TO TREE-STATUS.

       INSERT-NODES.
           DISPLAY "Inserting nodes: 50, 30, 70, 20, 40, 60, 80"
           PERFORM INSERT-NODE WITH TEST AFTER VARYING NODE-KEY
               FROM 50 BY -10
               UNTIL NODE-KEY < 10
           END-PERFORM.

       INSERT-NODE.
           IF TREE-EMPTY THEN
               MOVE NODE-KEY TO ROOT-NODE
               MOVE 'N' TO TREE-STATUS
               DISPLAY "Root node inserted: " NODE-KEY
           ELSE
               PERFORM INSERT-RECURSIVE
           END-IF.

       INSERT-RECURSIVE.
           MOVE ROOT-NODE TO CURRENT-NODE
           PERFORM UNTIL CURRENT-NODE = ZERO OR NODE-FOUND
               IF NODE-KEY < CURRENT-NODE THEN
                   IF NODE-LEFT = ZERO THEN
                       MOVE NODE-KEY TO NODE-LEFT
                       DISPLAY "Inserted " NODE-KEY " to left of " CURRENT-NODE
                       GO TO INSERT-RECURSIVE-EXIT
                   ELSE
                       MOVE NODE-LEFT TO CURRENT-NODE
                   END-IF
               ELSE
                   IF NODE-RIGHT = ZERO THEN
                       MOVE NODE-KEY TO NODE-RIGHT
                       DISPLAY "Inserted " NODE-KEY " to right of " CURRENT-NODE
                       GO TO INSERT-RECURSIVE-EXIT
                   ELSE
                       MOVE NODE-RIGHT TO CURRENT-NODE
                   END-IF
               END-IF
           END-PERFORM.

       INSERT-RECURSIVE-EXIT.
           EXIT.

       SEARCH-NODES.
           DISPLAY "Searching for nodes: 40, 25, 70"
           MOVE 40 TO SEARCH-KEY
           PERFORM SEARCH-NODE
           IF NODE-FOUND THEN
               DISPLAY "Node " SEARCH-KEY " found"
           ELSE
               DISPLAY "Node " SEARCH-KEY " not found"
           END-IF

           MOVE 25 TO SEARCH-KEY
           PERFORM SEARCH-NODE
           IF NODE-FOUND THEN
               DISPLAY "Node " SEARCH-KEY " found"
           ELSE
               DISPLAY "Node " SEARCH-KEY " not found"
           END-IF

           MOVE 70 TO SEARCH-KEY
           PERFORM SEARCH-NODE
           IF NODE-FOUND THEN
               DISPLAY "Node " SEARCH-KEY " found"
           ELSE
               DISPLAY "Node " SEARCH-KEY " not found"
           END-IF.

       SEARCH-NODE.
           MOVE ROOT-NODE TO CURRENT-NODE
           MOVE 'N' TO FOUND-FLAG

           PERFORM UNTIL CURRENT-NODE = ZERO OR NODE-FOUND
               IF SEARCH-KEY = CURRENT-NODE THEN
                   MOVE 'Y' TO FOUND-FLAG
                   GO TO SEARCH-NODE-EXIT
               ELSE IF SEARCH-KEY < CURRENT-NODE THEN
                   MOVE NODE-LEFT TO CURRENT-NODE
               ELSE
                   MOVE NODE-RIGHT TO CURRENT-NODE
               END-IF
           END-PERFORM.

       SEARCH-NODE-EXIT.
           EXIT.

       DISPLAY-TREE.
           DISPLAY "Tree structure:"
           IF TREE-EMPTY THEN
               DISPLAY "Tree is empty"
           ELSE
               DISPLAY "Root: " ROOT-NODE
               PERFORM DISPLAY-RECURSIVE VARYING NODE-KEY FROM ROOT-NODE BY 0
           END-IF.

       DISPLAY-RECURSIVE.
           IF CURRENT-NODE NOT = ZERO THEN
               DISPLAY "Node: " CURRENT-NODE
               IF NODE-LEFT NOT = ZERO THEN
                   DISPLAY "  Left child: " NODE-LEFT
               END-IF
               IF NODE-RIGHT NOT = ZERO THEN
                   DISPLAY "  Right child: " NODE-RIGHT
               END-IF
           END-IF.
```

## Key Features of this Implementation:

1. **Node Structure**: Each node contains a key value and pointers to left and right children
2. **Insertion Algorithm**: 
   - Uses recursive approach to find correct position
   - Maintains BST property (left < parent < right)
3. **Search Algorithm**:
   - Binary search approach for efficient lookup
   - Returns found/not-found status
4. **Tree Operations**:
   - Initialization of empty tree
   - Node insertion
   - Node searching
   - Tree display

## How it Works:

1. **Initialization**: Creates an empty binary search tree
2. **Insertion**: Adds nodes following BST rules (smaller values to left, larger to right)
3. **Search**: Traverses the tree comparing keys to find target value
4. **Display**: Shows tree structure with node relationships

This implementation demonstrates fundamental BST operations in COBOL, showing how traditional algorithms can be adapted for the procedural nature of COBOL programming.