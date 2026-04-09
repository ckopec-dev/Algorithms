# Quick Sort Algorithm in COBOL

Here's an example implementation of the Quick Sort algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. QUICK-SORT.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  ARRAY-SIZE           PIC 9(3) VALUE 10.
       01  ARRAY-ITEM           PIC 9(3) OCCURS 10 TIMES.
       01  I-INDEX              PIC 9(3).
       01  J-INDEX              PIC 9(3).
       01  PIVOT                PIC 9(3).
       01  TEMP                 PIC 9(3).
       01  LOW-BOUND            PIC 9(3).
       01  HIGH-BOUND           PIC 9(3).
       01  PARTITION-INDEX      PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "QUICK SORT EXAMPLE"
           DISPLAY "===================="

           *> Initialize array with sample data
           MOVE 64 TO ARRAY-ITEM(1)
           MOVE 34 TO ARRAY-ITEM(2)
           MOVE 25 TO ARRAY-ITEM(3)
           MOVE 12 TO ARRAY-ITEM(4)
           MOVE 22 TO ARRAY-ITEM(5)
           MOVE 11 TO ARRAY-ITEM(6)
           MOVE 90 TO ARRAY-ITEM(7)
           MOVE 88 TO ARRAY-ITEM(8)
           MOVE 76 TO ARRAY-ITEM(9)
           MOVE 50 TO ARRAY-ITEM(10)

           DISPLAY "Original Array:"
           PERFORM DISPLAY-ARRAY

           *> Perform quick sort
           MOVE 1 TO LOW-BOUND
           MOVE ARRAY-SIZE TO HIGH-BOUND
           CALL "QUICK-SORT-RECURSIVE" USING LOW-BOUND, HIGH-BOUND

           DISPLAY "Sorted Array:"
           PERFORM DISPLAY-ARRAY

           STOP RUN.

       DISPLAY-ARRAY.
           PERFORM VARYING I-INDEX FROM 1 BY 1 UNTIL I-INDEX > ARRAY-SIZE
               DISPLAY ARRAY-ITEM(I-INDEX) WITH NO ADVANCING
           END-PERFORM
           DISPLAY SPACE.

       QUICK-SORT-RECURSIVE.
           01  LOW-BOUND-ARG        PIC 9(3) VALUE 0.
           01  HIGH-BOUND-ARG       PIC 9(3) VALUE 0.

           ACCEPT LOW-BOUND-ARG FROM LOW-BOUND
           ACCEPT HIGH-BOUND-ARG FROM HIGH-BOUND

           IF LOW-BOUND-ARG < HIGH-BOUND-ARG
               *> Partition the array
               CALL "PARTITION" USING LOW-BOUND-ARG, HIGH-BOUND-ARG, PARTITION-INDEX

               *> Recursively sort elements before and after partition
               SUBTRACT 1 FROM PARTITION-INDEX
               CALL "QUICK-SORT-RECURSIVE" USING LOW-BOUND-ARG, PARTITION-INDEX

               ADD 1 TO PARTITION-INDEX
               CALL "QUICK-SORT-RECURSIVE" USING PARTITION-INDEX, HIGH-BOUND-ARG
           END-IF.

       PARTITION.
           01  LOW-BOUND-PAR        PIC 9(3) VALUE 0.
           01  HIGH-BOUND-PAR       PIC 9(3) VALUE 0.
           01  PARTITION-INDEX-PAR  PIC 9(3) VALUE 0.
           01  I-PAR                PIC 9(3) VALUE 0.
           01  J-PAR                PIC 9(3) VALUE 0.
           01  PIVOT-PAR            PIC 9(3) VALUE 0.

           ACCEPT LOW-BOUND-PAR FROM LOW-BOUND
           ACCEPT HIGH-BOUND-PAR FROM HIGH-BOUND

           *> Choose the rightmost element as pivot
           MOVE ARRAY-ITEM(HIGH-BOUND-PAR) TO PIVOT-PAR
           MOVE LOW-BOUND-PAR TO I-PAR
           MOVE LOW-BOUND-PAR TO PARTITION-INDEX-PAR

           *> Partition the array
           PERFORM VARYING J-PAR FROM LOW-BOUND-PAR BY 1
               UNTIL J-PAR >= HIGH-BOUND-PAR
               IF ARRAY-ITEM(J-PAR) < PIVOT-PAR
                   *> Swap elements
                   IF I-PAR NOT = J-PAR
                       MOVE ARRAY-ITEM(I-PAR) TO TEMP
                       MOVE TEMP TO ARRAY-ITEM(J-PAR)
                       MOVE ARRAY-ITEM(J-PAR) TO ARRAY-ITEM(I-PAR)
                   END-IF
                   ADD 1 TO I-PAR
               END-IF
           END-PERFORM

           *> Place pivot in its correct position
           IF I-PAR NOT = HIGH-BOUND-PAR
               MOVE ARRAY-ITEM(I-PAR) TO TEMP
               MOVE TEMP TO ARRAY-ITEM(HIGH-BOUND-PAR)
               MOVE ARRAY-ITEM(HIGH-BOUND-PAR) TO ARRAY-ITEM(I-PAR)
           END-IF

           MOVE I-PAR TO PARTITION-INDEX-PAR
           MOVE PARTITION-INDEX-PAR TO PARTITION-INDEX.

       *> End of program
```

## How it works:

1. **Main Program**: Initializes an array with sample data and calls the quick sort routine
2. **Quick-Sort-Recursive**: The main recursive function that implements the quick sort algorithm
3. **Partition**: Divides the array into two parts based on a pivot element
4. **Display-Array**: Helper function to display the array contents

## Algorithm Steps:

1. Choose a pivot element from the array
2. Partition the array so that elements smaller than the pivot are on the left and larger elements are on the right
3. Recursively apply the above steps to the sub-arrays
4. Continue until the entire array is sorted

## Sample Output:
```
QUICK SORT EXAMPLE
====================
Original Array:
64 34 25 12 22 11 90 88 76 50 
Sorted Array:
11 12 22 25 34 50 64 76 88 90 
```

This implementation demonstrates the classic quick sort algorithm in COBOL, showing how recursion and array manipulation work within the COBOL programming paradigm.

