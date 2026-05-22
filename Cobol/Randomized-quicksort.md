```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RANDOMIZED-QUICKSORT.
       AUTHOR. AI-ASSISTANT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ARRAY-SIZE        PIC 9(3) VALUE 10.
       01  ARRAY-ITEM        PIC 9(3) OCCURS 10 TIMES.
       01  I                 PIC 9(3).
       01  J                 PIC 9(3).
       01  PIVOT             PIC 9(3).
       01  TEMP              PIC 9(3).
       01  LOW               PIC 9(3).
       01  HIGH              PIC 9(3).
       01  RANDOM-SEED       PIC 9(9) VALUE 123456789.
       01  RANDOM-NUMBER     PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-ARRAY
           PERFORM DISPLAY-ARRAY
           MOVE 1 TO LOW
           MOVE ARRAY-SIZE TO HIGH
           PERFORM QUICKSORT-RECURSIVE
           PERFORM DISPLAY-ARRAY
           STOP RUN.

       INITIALIZE-ARRAY.
           MOVE 56 TO ARRAY-ITEM(1)
           MOVE 34 TO ARRAY-ITEM(2)
           MOVE 78 TO ARRAY-ITEM(3)
           MOVE 23 TO ARRAY-ITEM(4)
           MOVE 91 TO ARRAY-ITEM(5)
           MOVE 12 TO ARRAY-ITEM(6)
           MOVE 67 TO ARRAY-ITEM(7)
           MOVE 45 TO ARRAY-ITEM(8)
           MOVE 89 TO ARRAY-ITEM(9)
           MOVE 33 TO ARRAY-ITEM(10).

       DISPLAY-ARRAY.
           DISPLAY "ARRAY: "
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               DISPLAY ARRAY-ITEM(I) WITH NO ADVANCING
           END-PERFORM
           DISPLAY " ".

       QUICKSORT-RECURSIVE.
           IF LOW < HIGH
               PERFORM PARTITION-AND-SORT
               PERFORM QUICKSORT-RECURSIVE
               MOVE LOW TO I
               MOVE PIVOT TO J
               ADD 1 TO J
               PERFORM QUICKSORT-RECURSIVE
           END-IF.

       PARTITION-AND-SORT.
           PERFORM RANDOMIZE-PIVOT
           PERFORM SWAP-PIVOT-TO-END
           PERFORM PARTITION-ARRAY
           PERFORM SWAP-PIVOT-TO-POSITION.

       RANDOMIZE-PIVOT.
           COMPUTE RANDOM-NUMBER = FUNCTION RANDOM(RANDOM-SEED)
           COMPUTE RANDOM-NUMBER = (RANDOM-NUMBER * (HIGH - LOW + 1)) + LOW
           MOVE RANDOM-NUMBER TO PIVOT.

       SWAP-PIVOT-TO-END.
           MOVE ARRAY-ITEM(PIVOT) TO TEMP
           MOVE ARRAY-ITEM(HIGH) TO ARRAY-ITEM(PIVOT)
           MOVE TEMP TO ARRAY-ITEM(HIGH).

       PARTITION-ARRAY.
           MOVE LOW TO I
           MOVE LOW TO J
           PERFORM VARYING I FROM LOW BY 1 UNTIL I >= HIGH
               IF ARRAY-ITEM(I) < ARRAY-ITEM(HIGH)
                   PERFORM SWAP-ITEMS
                   ADD 1 TO J
               END-IF
           END-PERFORM.

       SWAP-ITEMS.
           MOVE ARRAY-ITEM(I) TO TEMP
           MOVE ARRAY-ITEM(J) TO ARRAY-ITEM(I)
           MOVE TEMP TO ARRAY-ITEM(J).

       SWAP-PIVOT-TO-POSITION.
           MOVE ARRAY-ITEM(HIGH) TO TEMP
           MOVE ARRAY-ITEM(J) TO ARRAY-ITEM(HIGH)
           MOVE TEMP TO ARRAY-ITEM(J).
```

This COBOL implementation demonstrates the Randomized QuickSort algorithm with the following key components:

1. **Array Initialization**: Creates and populates an array with sample data
2. **Random Pivot Selection**: Uses a random number generator to select pivot element
3. **Partitioning**: Rearranges array elements around the pivot
4. **Recursive Sorting**: Applies quicksort to subarrays
5. **Swapping Operations**: Implements element swapping for partitioning

The algorithm randomizes the pivot selection to avoid worst-case performance on sorted arrays, making it more efficient on average. The program shows the array before and after sorting to demonstrate the sorting process.

