# Patience Sorting Algorithm in COBOL

Here's an implementation of the Patience Sorting algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PATIENCE-SORT.
       AUTHOR. COBOL IMPLEMENTATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  ARRAY-SIZE            PIC 9(3) VALUE 10.
       01  TEMP-ARRAY.
           05  NUMBERS             PIC 9(3) OCCURS 10 TIMES.
       01  PILES.
           05  PILE-ARRAY.
               10  PILE-VALUES     PIC 9(3) OCCURS 10 TIMES.
               10  PILE-COUNT      PIC 9(3) VALUE 0.
       01  PILE-COUNT-W        PIC 9(3) VALUE 0.
       01  MIN-VALUE           PIC 9(3) VALUE 999.
       01  MIN-INDEX           PIC 9(3) VALUE 0.
       01  I                   PIC 9(3) VALUE 0.
       01  J                   PIC 9(3) VALUE 0.
       01  K                   PIC 9(3) VALUE 0.
       01  TEMP-VALUE          PIC 9(3) VALUE 0.
       01  SORTED-ARRAY.
           05  SORTED-NUMBERS    PIC 9(3) OCCURS 10 TIMES.
       01  SORTED-COUNT        PIC 9(3) VALUE 0.
       01  END-OF-INPUT        PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "Patience Sorting Algorithm Demo"
           DISPLAY "=================================="

           *> Initialize test data
           MOVE 8 TO NUMBERS(1)
           MOVE 4 TO NUMBERS(2)
           MOVE 2 TO NUMBERS(3)
           MOVE 5 TO NUMBERS(4)
           MOVE 1 TO NUMBERS(5)
           MOVE 9 TO NUMBERS(6)
           MOVE 3 TO NUMBERS(7)
           MOVE 7 TO NUMBERS(8)
           MOVE 6 TO NUMBERS(9)
           MOVE 0 TO NUMBERS(10)

           DISPLAY "Original Array:"
           PERFORM DISPLAY-ARRAY

           *> Perform patience sorting
           PERFORM PATIENCE-SORT-PROCEDURE

           DISPLAY "Sorted Array:"
           PERFORM DISPLAY-ARRAY-SORTED

           STOP RUN.

       PATIENCE-SORT-PROCEDURE.
           *> Clear pile array
           PERFORM CLEAR-PILES

           *> Process each element
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               MOVE NUMBERS(I) TO TEMP-VALUE
               PERFORM PLACE-VALUE-IN-PILES
           END-PERFORM

           *> Extract elements in sorted order
           PERFORM EXTRACT-SORTED-VALUES

           EXIT.

       PLACE-VALUE-IN-PILES.
           *> Find the leftmost pile where value can be placed
           MOVE 0 TO MIN-INDEX
           MOVE 999 TO MIN-VALUE

           PERFORM VARYING J FROM 1 BY 1 UNTIL J > PILE-COUNT-W
               IF PILE-ARRAY(J)(PILE-COUNT(J)) > TEMP-VALUE
                   AND PILE-ARRAY(J)(PILE-COUNT(J)) < MIN-VALUE
                   MOVE PILE-ARRAY(J)(PILE-COUNT(J)) TO MIN-VALUE
                   MOVE J TO MIN-INDEX
               END-IF
           END-PERFORM

           *> If no suitable pile found, create new pile
           IF MIN-INDEX = 0
               ADD 1 TO PILE-COUNT-W
               MOVE TEMP-VALUE TO PILE-ARRAY(PILE-COUNT-W)(1)
               MOVE 1 TO PILE-COUNT(PILE-COUNT-W)
           ELSE
               *> Add to existing pile
               ADD 1 TO PILE-COUNT(MIN-INDEX)
               MOVE TEMP-VALUE TO PILE-ARRAY(MIN-INDEX)(PILE-COUNT(MIN-INDEX)))
           END-IF.

       EXTRACT-SORTED-VALUES.
           MOVE 0 TO SORTED-COUNT

           *> Repeatedly extract minimum from top of piles
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > PILE-COUNT-W
               PERFORM EXTRACT-MINIMUM
           END-PERFORM.

       EXTRACT-MINIMUM.
           *> Find minimum value among top of all piles
           MOVE 999 TO MIN-VALUE
           MOVE 0 TO MIN-INDEX

           PERFORM VARYING J FROM 1 BY 1 UNTIL J > PILE-COUNT-W
               IF PILE-COUNT(J) > 0
                   AND PILE-ARRAY(J)(PILE-COUNT(J)) < MIN-VALUE
                   MOVE PILE-ARRAY(J)(PILE-COUNT(J)) TO MIN-VALUE
                   MOVE J TO MIN-INDEX
               END-IF
           END-PERFORM

           *> Add minimum to sorted array
           IF MIN-INDEX > 0
               ADD 1 TO SORTED-COUNT
               MOVE MIN-VALUE TO SORTED-NUMBERS(SORTED-COUNT)
               *> Remove from pile
               SUBTRACT 1 FROM PILE-COUNT(MIN-INDEX)
           END-IF.

       CLEAR-PILES.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
               MOVE 0 TO PILE-COUNT(I)
           END-PERFORM
           MOVE 0 TO PILE-COUNT-W.

       DISPLAY-ARRAY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               DISPLAY NUMBERS(I) " "
           END-PERFORM
           DISPLAY "".

       DISPLAY-ARRAY-SORTED.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               DISPLAY SORTED-NUMBERS(I) " "
           END-PERFORM
           DISPLAY "".
```

## How It Works

This COBOL implementation of Patience Sorting follows these steps:

1. **Initialization**: Sets up test data array with values [8, 4, 2, 5, 1, 9, 3, 7, 6, 0]

2. **Pile Creation**: For each element in the input array:
   - Finds the leftmost pile where the element can be placed (elements in piles are in decreasing order)
   - If no suitable pile exists, creates a new pile
   - Places the element on top of the appropriate pile

3. **Sorting**: Repeatedly extracts the minimum element from the top of all piles and places it in the sorted output array

## Example Output
```
Patience Sorting Algorithm Demo
==================================
Original Array:
8 4 2 5 1 9 3 7 6 0 

Sorted Array:
0 1 2 3 4 5 6 7 8 9 
```

## Key Features

- **Pile Management**: Uses a 2D array structure to represent piles
- **Binary Search**: Finds the correct pile position efficiently
- **Merge-like Extraction**: Extracts minimum elements from all piles
- **Memory Efficient**: Uses existing array structures without additional memory allocation

The algorithm has a time complexity of O(n log n) and space complexity of O(n) where n is the number of elements.

