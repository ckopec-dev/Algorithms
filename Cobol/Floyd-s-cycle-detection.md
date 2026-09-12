# Floyd's Cycle Detection Algorithm in COBOL

Floyd's Cycle Detection (also known as the "tortoise and hare" algorithm) can be implemented in COBOL to detect cycles in linked lists or sequences.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FLOYDS-CYCLE-DETECTION.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  NODE-STRUCTURE.
          05  NODE-DATA    PIC 9(4).
          05  NODE-NEXT    POINTER.
          05  NODE-VISITED PIC X VALUE 'N'.

       01  TURTLE-POINTER   POINTER.
       01  HARE-POINTER     POINTER.
       01  CURRENT-POINTER  POINTER.
       01  TEMP-POINTER     POINTER.
       
       01  FOUND-CYCLE      PIC X VALUE 'N'.
       01  CYCLE-STARTED    PIC X VALUE 'N'.
       01  ITERATION-COUNT  PIC 9(4) VALUE 0.
       01  MAX-ITERATIONS   PIC 9(4) VALUE 1000.

       01  NODE-LIST.
          05  NODE-ITEM      OCCURS 10 TIMES INDEXED BY I.
             10  ITEM-DATA   PIC 9(4).
             10  ITEM-NEXT   PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-LIST
           PERFORM CREATE-LINKED-LIST
           PERFORM DETECT-CYCLE
           PERFORM DISPLAY-RESULTS
           STOP RUN.

       INITIALIZE-LIST.
           MOVE ZERO TO ITERATION-COUNT
           MOVE 'N' TO FOUND-CYCLE
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
               MOVE ZERO TO ITEM-NEXT(I)
           END-PERFORM.

       CREATE-LINKED-LIST.
           MOVE 100 TO ITEM-DATA(1)
           MOVE 200 TO ITEM-DATA(2)
           MOVE 300 TO ITEM-DATA(3)
           MOVE 400 TO ITEM-DATA(4)
           MOVE 500 TO ITEM-DATA(5)
           MOVE 600 TO ITEM-DATA(6)
           MOVE 700 TO ITEM-DATA(7)
           MOVE 800 TO ITEM-DATA(8)
           MOVE 900 TO ITEM-DATA(9)
           MOVE 1000 TO ITEM-DATA(10)

           MOVE 2 TO ITEM-NEXT(1)    *> 1 -> 2
           MOVE 3 TO ITEM-NEXT(2)    *> 2 -> 3
           MOVE 4 TO ITEM-NEXT(3)    *> 3 -> 4
           MOVE 5 TO ITEM-NEXT(4)    *> 4 -> 5
           MOVE 6 TO ITEM-NEXT(5)    *> 5 -> 6
           MOVE 7 TO ITEM-NEXT(6)    *> 6 -> 7
           MOVE 8 TO ITEM-NEXT(7)    *> 7 -> 8
           MOVE 9 TO ITEM-NEXT(8)    *> 8 -> 9
           MOVE 3 TO ITEM-NEXT(9)    *> 9 -> 3 (creates cycle)
           MOVE 0 TO ITEM-NEXT(10)

           DISPLAY "Created linked list with potential cycle:"
           PERFORM DISPLAY-LIST.

       DETECT-CYCLE.
           *> Initialize pointers
           MOVE 1 TO I
           SET TURTLE-POINTER TO FUNCTION ADDRESS OF ITEM-DATA(I)
           SET HARE-POINTER TO FUNCTION ADDRESS OF ITEM-DATA(I)

           PERFORM VARYING ITERATION-COUNT FROM 1 BY 1 
               UNTIL ITERATION-COUNT > MAX-ITERATIONS OR FOUND-CYCLE = 'Y'
               *> Move turtle one step
               ADD 1 TO I GIVING J
               IF ITEM-NEXT(J) NOT = 0
                   MOVE ITEM-NEXT(J) TO I
                   SET TURTLE-POINTER TO FUNCTION ADDRESS OF ITEM-DATA(I)
               END-IF

               *> Move hare two steps
               ADD 1 TO I GIVING J
               IF ITEM-NEXT(J) NOT = 0
                   MOVE ITEM-NEXT(J) TO I
                   SET HARE-POINTER TO FUNCTION ADDRESS OF ITEM-DATA(I)
                   ADD 1 TO I GIVING J
                   IF ITEM-NEXT(J) NOT = 0
                       MOVE ITEM-NEXT(J) TO I
                       SET HARE-POINTER TO FUNCTION ADDRESS OF ITEM-DATA(I)
                   END-IF
               END-IF

               *> Check if pointers meet (cycle detected)
               IF TURTLE-POINTER = HARE-POINTER
                   MOVE 'Y' TO FOUND-CYCLE
                   DISPLAY "Cycle detected at position: " I
               END-IF
           END-PERFORM.

       DISPLAY-LIST.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
               DISPLAY "Node " I ": Data=" ITEM-DATA(I)
                       ", Next=" ITEM-NEXT(I)
           END-PERFORM.

       DISPLAY-RESULTS.
           IF FOUND-CYCLE = 'Y'
               DISPLAY "RESULT: Cycle detected in the linked list"
           ELSE
               DISPLAY "RESULT: No cycle found in the linked list"
           END-IF
           DISPLAY "Iterations performed: " ITERATION-COUNT.

       END PROGRAM FLOYDS-CYCLE-DETECTION.
```

## Explanation of Key Components:

1. **Data Structure**: The program uses a simple array-based linked list structure with data and next pointers

2. **Algorithm Logic**:
   - Uses two pointers (turtle and hare)
   - Turtle moves one step at a time
   - Hare moves two steps at a time
   - If they meet, a cycle exists

3. **Key Features**:
   - Initialize pointers to start of list
   - Move turtle one step, hare two steps
   - Compare pointer addresses for cycle detection
   - Include iteration limit to prevent infinite loops

4. **Output**: 
   - Displays whether a cycle was detected
   - Shows the position where cycle was found
   - Reports number of iterations performed

This implementation demonstrates the classic Floyd's cycle detection algorithm adapted for COBOL's structured approach to data handling and pointer operations.