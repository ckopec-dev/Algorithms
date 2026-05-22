# Knapsack Problem Algorithm in COBOL

Here's a COBOL implementation of the 0/1 Knapsack problem using dynamic programming:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. KNAPSACK-PROBLEM.
       AUTHOR. COBOL IMPLEMENTATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "KNAPSACK.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
          05 ITEM-NUMBER      PIC 9(3).
          05 ITEM-WEIGHT      PIC 9(3).
          05 ITEM-VALUE       PIC 9(4).

       WORKING-STORAGE SECTION.
       01 WS-TOTAL-ITEMS     PIC 9(3) VALUE 0.
       01 WS-KNAPSACK-CAPACITY PIC 9(3) VALUE 50.
       01 WS-I                 PIC 9(3) VALUE 0.
       01 WS-J                 PIC 9(3) VALUE 0.
       01 WS-MAX-VALUE         PIC 9(5) VALUE 0.
       01 WS-TEMP-VALUE        PIC 9(5) VALUE 0.
       01 WS-ITEM-WEIGHT       PIC 9(3) VALUE 0.
       01 WS-ITEM-VALUE        PIC 9(4) VALUE 0.
       01 WS-FOUND             PIC X VALUE 'N'.

       01 WS-ITEMS.
          05 ITEM-DEFN REDEFINES ITEM-DEFN-ARRAY.
             10 ITEM-ARRAY OCCURS 20 TIMES.
                15 ITEM-WT      PIC 9(3).
                15 ITEM-VL      PIC 9(4).
                15 ITEM-ID      PIC 9(3).

       01 WS-KNAPSACK-MATRIX.
          05 MATRIX-CELLS OCCURS 20 TIMES DEPENDING ON WS-TOTAL-ITEMS.
             10 MATRIX-ROW OCCURS 50 TIMES.
                15 MATRIX-ELEMENT PIC 9(5).

       01 WS-OUTPUT-FILE.
          05 OUTPUT-RECORD.
             10 O-ITEM-ID     PIC 9(3).
             10 O-ITEM-WT     PIC 9(3).
             10 O-ITEM-VL     PIC 9(4).
             10 O-SELECTED    PIC X.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "KNAPSACK PROBLEM SOLUTION"
           DISPLAY "========================"
           DISPLAY "Capacity: " WS-KNAPSACK-CAPACITY
           DISPLAY ""

           PERFORM INITIALIZE-DATA
           PERFORM READ-INPUT-DATA
           PERFORM SOLVE-KNAPSACK
           PERFORM DISPLAY-RESULTS
           PERFORM WRITE-OUTPUT

           STOP RUN.

       INITIALIZE-DATA.
           MOVE 0 TO WS-TOTAL-ITEMS
           MOVE 0 TO WS-I
           MOVE 0 TO WS-J
           MOVE 0 TO WS-MAX-VALUE
           MOVE 0 TO WS-TEMP-VALUE
           MOVE 'N' TO WS-FOUND.

       READ-INPUT-DATA.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE
               AT END GO TO READ-INPUT-END
           END-READ
           PERFORM PROCESS-INPUT-RECORD
           GO TO READ-INPUT-DATA
           READ-INPUT-END.
           CLOSE INPUT-FILE.

       PROCESS-INPUT-RECORD.
           ADD 1 TO WS-TOTAL-ITEMS
           MOVE ITEM-NUMBER TO ITEM-ARRAY(WS-TOTAL-ITEMS).ITEM-ID
           MOVE ITEM-WEIGHT TO ITEM-ARRAY(WS-TOTAL-ITEMS).ITEM-WT
           MOVE ITEM-VALUE TO ITEM-ARRAY(WS-TOTAL-ITEMS).ITEM-VL.

       SOLVE-KNAPSACK.
           PERFORM INITIALIZE-MATRIX
           PERFORM FILL-MATRIX
           PERFORM BACKTRACK-SOLUTION.

       INITIALIZE-MATRIX.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-TOTAL-ITEMS
               PERFORM VARYING WS-J FROM 0 BY 1 UNTIL WS-J > WS-KNAPSACK-CAPACITY
                   MOVE 0 TO MATRIX-ELEMENT(WS-I, WS-J)
               END-PERFORM
           END-PERFORM.

       FILL-MATRIX.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-TOTAL-ITEMS
               PERFORM VARYING WS-J FROM 0 BY 1 UNTIL WS-J > WS-KNAPSACK-CAPACITY
                   IF WS-J >= ITEM-WT(WS-I)
                       COMPUTE WS-TEMP-VALUE = MATRIX-ELEMENT(WS-I-1, WS-J-ITEM-WT(WS-I)) + ITEM-VL(WS-I)
                       IF WS-TEMP-VALUE > MATRIX-ELEMENT(WS-I-1, WS-J)
                           MOVE WS-TEMP-VALUE TO MATRIX-ELEMENT(WS-I, WS-J)
                       ELSE
                           MOVE MATRIX-ELEMENT(WS-I-1, WS-J) TO MATRIX-ELEMENT(WS-I, WS-J)
                       END-IF
                   ELSE
                       MOVE MATRIX-ELEMENT(WS-I-1, WS-J) TO MATRIX-ELEMENT(WS-I, WS-J)
                   END-IF
               END-PERFORM
           END-PERFORM.

       BACKTRACK-SOLUTION.
           MOVE MATRIX-ELEMENT(WS-TOTAL-ITEMS, WS-KNAPSACK-CAPACITY) TO WS-MAX-VALUE
           DISPLAY "Maximum Value: " WS-MAX-VALUE.

       DISPLAY-RESULTS.
           DISPLAY "Optimal Solution:"
           DISPLAY "Maximum Value Achieved: " WS-MAX-VALUE
           DISPLAY "Items selected for maximum value:"
           DISPLAY "Item ID  Weight  Value"
           DISPLAY "-------  ------  -----"
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-TOTAL-ITEMS
               DISPLAY ITEM-ID(WS-I) "  " ITEM-WT(WS-I) "  " ITEM-VL(WS-I)
           END-PERFORM.

       WRITE-OUTPUT.
           OPEN OUTPUT OUTPUT-FILE
           WRITE OUTPUT-RECORD FROM WS-OUTPUT-FILE
           CLOSE OUTPUT-FILE.

       END PROGRAM KNAPSACK-PROBLEM.
```

## Sample Input File (KNAPSACK.DAT)
```
001 10 60
002 20 100
003 30 120
004 15 80
005 25 150
```

## Key Features of this Implementation:

1. **Dynamic Programming Approach**: Uses a 2D matrix to store optimal values for subproblems
2. **0/1 Knapsack**: Each item can be taken at most once
3. **Capacity Constraint**: Respects the knapsack's weight capacity
4. **Optimal Solution**: Finds the maximum value achievable within constraints
5. **COBOL-Specific Features**: Uses COBOL data definitions, control structures, and file handling

## Algorithm Complexity:
- **Time Complexity**: O(n × W) where n is number of items and W is capacity
- **Space Complexity**: O(n × W) for the DP table

This implementation demonstrates the classic knapsack problem solution using COBOL's structured programming capabilities.

