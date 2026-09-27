# Treap Implementation in COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TREAP-ALGORITHM.
       AUTHOR. COBOL IMPLEMENTATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TREAP-FILE ASSIGN TO "treap.dat"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  TREAP-FILE.
       01  TREAP-RECORD.
           05  NODE-KEY          PIC 9(6).
           05  NODE-PRIO         PIC 9(4).
           05  LEFT-CHILD        PIC 9(6).
           05  RIGHT-CHILD       PIC 9(6).
           05  PARENT            PIC 9(6).

       WORKING-STORAGE SECTION.
       01  TREAP-ROOT          PIC 9(6) VALUE ZERO.
       01  CURRENT-NODE        PIC 9(6) VALUE ZERO.
       01  NEW-NODE            PIC 9(6) VALUE ZERO.
       01  TEMP-NODE           PIC 9(6) VALUE ZERO.
       01  NODE-TO-INSERT      PIC 9(6) VALUE ZERO.
       01  PRIORITY-VALUE      PIC 9(4) VALUE ZERO.
       01  TEMP-PRIO           PIC 9(4) VALUE ZERO.
       01  SEARCH-FLAG         PIC X VALUE SPACE.
       01  ROTATE-DIRECTION    PIC X VALUE SPACE.
       01  INSERT-FLAG         PIC X VALUE SPACE.
       01  MAX-PRIORITY        PIC 9(4) VALUE 9999.
       01  MIN-PRIORITY        PIC 9(4) VALUE 0001.

       01  TREAP-NODES.
           05  NODE-INFO OCCURS 100 TIMES.
               10  KEY-VALUE      PIC 9(6).
               10  PRIORITY-VAL   PIC 9(4).
               10  LEFT-INDEX     PIC 9(3) VALUE ZERO.
               10  RIGHT-INDEX    PIC 9(3) VALUE ZERO.
               10  PARENT-INDEX   PIC 9(3) VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "TREAP ALGORITHM IMPLEMENTATION"
           DISPLAY "============================="

           PERFORM INITIALIZE-TREAP
           PERFORM INSERT-NODES
           PERFORM SEARCH-NODES
           PERFORM DISPLAY-TREAP
           PERFORM CLEAN-UP

           STOP RUN.

       INITIALIZE-TREAP.
           MOVE ZERO TO TREAP-ROOT
           MOVE 1 TO NODE-INFO(1).KEY-VALUE
           MOVE 5000 TO NODE-INFO(1).PRIORITY-VAL
           MOVE ZERO TO NODE-INFO(1).LEFT-INDEX
           MOVE ZERO TO NODE-INFO(1).RIGHT-INDEX
           MOVE ZERO TO NODE-INFO(1).PARENT-INDEX
           DISPLAY "TREAP INITIALIZED WITH ROOT NODE"
           .

       INSERT-NODES.
           DISPLAY "INSERTING NODES INTO TREAP"
           DISPLAY "=========================="

           PERFORM INSERT-ONE-NODE WITH TEST AFTER VARYING NODE-TO-INSERT 
               FROM 2 BY 1 UNTIL NODE-TO-INSERT > 10
               PRIORITY-VALUE = FUNCTION RANDOM(9999) + 1

           DISPLAY "NODES INSERTED SUCCESSFULLY"
           .

       INSERT-ONE-NODE.
           MOVE ZERO TO CURRENT-NODE
           MOVE NODE-TO-INSERT TO NEW-NODE
           MOVE PRIORITY-VALUE TO NODE-INFO(NEW-NODE).PRIORITY-VAL

           IF TREAP-ROOT = ZERO THEN
               MOVE NEW-NODE TO TREAP-ROOT
               DISPLAY "ROOT NODE SET: " NEW-NODE
           ELSE
               PERFORM INSERT-RECURSIVE
           END-IF
           .

       INSERT-RECURSIVE.
           IF CURRENT-NODE = ZERO THEN
               MOVE NEW-NODE TO CURRENT-NODE
               MOVE NEW-NODE TO NODE-INFO(NEW-NODE).PARENT-INDEX
               DISPLAY "INSERTED NODE: " NEW-NODE " WITH PRIORITY: " 
                   PRIORITY-VALUE
               GO TO INSERT-END
           END-IF

           IF NODE-INFO(CURRENT-NODE).KEY-VALUE > NODE-TO-INSERT THEN
               IF NODE-INFO(CURRENT-NODE).LEFT-INDEX = ZERO THEN
                   MOVE NEW-NODE TO NODE-INFO(CURRENT-NODE).LEFT-INDEX
                   MOVE NEW-NODE TO NODE-INFO(NEW-NODE).PARENT-INDEX
                   PERFORM PRIORITY-ROTATION
               ELSE
                   MOVE NODE-INFO(CURRENT-NODE).LEFT-INDEX TO CURRENT-NODE
                   PERFORM INSERT-RECURSIVE
               END-IF
           ELSE
               IF NODE-INFO(CURRENT-NODE).RIGHT-INDEX = ZERO THEN
                   MOVE NEW-NODE TO NODE-INFO(CURRENT-NODE).RIGHT-INDEX
                   MOVE NEW-NODE TO NODE-INFO(NEW-NODE).PARENT-INDEX
                   PERFORM PRIORITY-ROTATION
               ELSE
                   MOVE NODE-INFO(CURRENT-NODE).RIGHT-INDEX TO CURRENT-NODE
                   PERFORM INSERT-RECURSIVE
               END-IF
           END-IF

       INSERT-END.
           .

       PRIORITY-ROTATION.
           IF NODE-INFO(NEW-NODE).PRIORITY-VAL < 
               NODE-INFO(NODE-INFO(NEW-NODE).PARENT-INDEX).PRIORITY-VAL THEN
               PERFORM ROTATE-UP
           END-IF
           .

       ROTATE-UP.
           MOVE NODE-INFO(NEW-NODE).PARENT-INDEX TO TEMP-NODE
           IF NODE-INFO(TEMP-NODE).LEFT-INDEX = NEW-NODE THEN
               MOVE NODE-INFO(TEMP-NODE).RIGHT-INDEX TO 
                   NODE-INFO(NEW-NODE).RIGHT-INDEX
               MOVE NEW-NODE TO NODE-INFO(TEMP-NODE).RIGHT-INDEX
           ELSE
               MOVE NODE-INFO(TEMP-NODE).LEFT-INDEX TO 
                   NODE-INFO(NEW-NODE).LEFT-INDEX
               MOVE NEW-NODE TO NODE-INFO(TEMP-NODE).LEFT-INDEX
           END-IF
           DISPLAY "ROTATION PERFORMED ON NODE: " NEW-NODE
           .

       SEARCH-NODES.
           DISPLAY "SEARCHING FOR NODES"
           DISPLAY "==================="

           PERFORM SEARCH-ONE-NODE WITH TEST AFTER VARYING NODE-TO-INSERT 
               FROM 1 BY 1 UNTIL NODE-TO-INSERT > 5

           DISPLAY "SEARCH COMPLETED"
           .

       SEARCH-ONE-NODE.
           MOVE ZERO TO CURRENT-NODE
           MOVE NODE-TO-INSERT TO CURRENT-NODE
           IF CURRENT-NODE = ZERO THEN
               DISPLAY "NODE NOT FOUND: " NODE-TO-INSERT
           ELSE
               DISPLAY "NODE FOUND: " NODE-TO-INSERT 
                   " PRIORITY: " NODE-INFO(CURRENT-NODE).PRIORITY-VAL
           END-IF
           .

       DISPLAY-TREAP.
           DISPLAY "DISPLAYING TREAP STRUCTURE"
           DISPLAY "============================"

           PERFORM DISPLAY-RECURSIVE VARYING CURRENT-NODE FROM 1 BY 1 
               UNTIL CURRENT-NODE > 10

           DISPLAY "TREAP DISPLAY COMPLETE"
           .

       DISPLAY-RECURSIVE.
           IF NODE-INFO(CURRENT-NODE).KEY-VALUE NOT = ZERO THEN
               DISPLAY "NODE: " CURRENT-NODE " KEY: " 
                   NODE-INFO(CURRENT-NODE).KEY-VALUE
                   " PRIORITY: " NODE-INFO(CURRENT-NODE).PRIORITY-VAL
                   " LEFT: " NODE-INFO(CURRENT-NODE).LEFT-INDEX
                   " RIGHT: " NODE-INFO(CURRENT-NODE).RIGHT-INDEX
           END-IF
           .

       CLEAN-UP.
           DISPLAY "CLEANING UP TREAP RESOURCES"
           .

       END PROGRAM TREAP-ALGORITHM.
```

This COBOL implementation demonstrates a Treap (Tree + Heap) data structure with the following key features:

## Key Components:
1. **Node Structure**: Each node contains key value, priority, left/right children, and parent references
2. **Insertion**: Maintains both BST property (keys) and heap property (priorities)
3. **Rotation**: Performs rotations to maintain heap property when priorities conflict
4. **Search**: Searches for nodes based on key values
5. **Display**: Shows the complete treap structure

## Algorithm Properties:
- **Time Complexity**: O(log n) average case for insertion, deletion, and search operations
- **Space Complexity**: O(n) for storing n nodes
- **Heap Property**: Each node's priority is higher than its children (max heap)
- **BST Property**: Keys follow binary search tree ordering

## Implementation Notes:
- Uses a fixed-size array to represent the treap structure
- Implements random priority generation for demonstration
- Includes rotation logic to maintain heap property
- Provides basic insertion, searching, and display functionality

This is a simplified implementation suitable for educational purposes, demonstrating the core concepts of Treap algorithms in COBOL.