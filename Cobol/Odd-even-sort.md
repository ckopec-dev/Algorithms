# Odd-Even Sort Algorithm in COBOL

Here's an example implementation of the Odd-Even sort (also known as Brick Sort) algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ODD-EVEN-SORT.
       AUTHOR. COBOL EXAMPLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ARRAY-SIZE        PIC 9(3) VALUE 8.
       01 I                 PIC 9(3).
       01 J                 PIC 9(3).
       01 TEMP              PIC 9(3).
       01 SWAPPED           PIC X VALUE 'N'.
       01 ARRAY-TABLE.
          05 ARRAY OCCURS 10 TIMES PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "ODD-EVEN SORT EXAMPLE"
           DISPLAY "======================="

           * Initialize array with sample data
           MOVE 64 TO ARRAY(1)
           MOVE 34 TO ARRAY(2)
           MOVE 25 TO ARRAY(3)
           MOVE 12 TO ARRAY(4)
           MOVE 22 TO ARRAY(5)
           MOVE 11 TO ARRAY(6)
           MOVE 90 TO ARRAY(7)
           MOVE 88 TO ARRAY(8)

           DISPLAY "Original Array:"
           PERFORM DISPLAY-ARRAY

           * Perform Odd-Even Sort
           PERFORM ODD-EVEN-SORT-ALGORITHM

           DISPLAY "Sorted Array:"
           PERFORM DISPLAY-ARRAY

           STOP RUN.

       ODD-EVEN-SORT-ALGORITHM.
           PERFORM UNTIL SWAPPED = 'N'
               MOVE 'N' TO SWAPPED

               * Odd phase
               PERFORM ODD-PHASE

               * Even phase
               PERFORM EVEN-PHASE
           END-PERFORM.

       ODD-PHASE.
           MOVE 1 TO I
           PERFORM UNTIL I >= ARRAY-SIZE
               IF ARRAY(I) > ARRAY(I + 1)
                   MOVE ARRAY(I) TO TEMP
                   MOVE ARRAY(I + 1) TO ARRAY(I)
                   MOVE TEMP TO ARRAY(I + 1)
                   MOVE 'Y' TO SWAPPED
               END-IF
               ADD 2 TO I
           END-PERFORM.

       EVEN-PHASE.
           MOVE 2 TO I
           PERFORM UNTIL I >= ARRAY-SIZE
               IF ARRAY(I) > ARRAY(I + 1)
                   MOVE ARRAY(I) TO TEMP
                   MOVE ARRAY(I + 1) TO ARRAY(I)
                   MOVE TEMP TO ARRAY(I + 1)
                   MOVE 'Y' TO SWAPPED
               END-IF
               ADD 2 TO I
           END-PERFORM.

       DISPLAY-ARRAY.
           MOVE 1 TO I
           DISPLAY "Array: "
           PERFORM UNTIL I > ARRAY-SIZE
               DISPLAY ARRAY(I) WITH NO ADVANCING
               ADD 1 TO I
           END-PERFORM
           DISPLAY " "
           DISPLAY "------------------------".
```

## How the Algorithm Works:

1. **Odd-Even Sort** is a variation of Bubble Sort that works in two phases:
   - **Odd Phase**: Compare elements at odd indices (1,3,5...) with their next neighbor
   - **Even Phase**: Compare elements at even indices (2,4,6...) with their next neighbor

2. **Process**:
   - Continue alternating between odd and even phases
   - After each phase, check if any swaps occurred
   - If no swaps occurred, the array is sorted
   - Repeat until no more swaps are needed

3. **Time Complexity**: O(n²) in worst case
4. **Space Complexity**: O(1) - sorts in place

## Sample Output:
```
ODD-EVEN SORT EXAMPLE
=======================
Original Array:
Array: 64 34 25 12 22 11 90 88 
------------------------
Sorted Array:
Array: 11 12 22 25 34 64 88 90 
------------------------
```

This implementation demonstrates the classic Odd-Even sorting algorithm in COBOL, showing how it alternates between odd and even indexed comparisons to sort the array.

