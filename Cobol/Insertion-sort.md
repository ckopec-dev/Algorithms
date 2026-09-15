# Insertion Sort Algorithm in COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSERTION-SORT.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01 ARRAY-SIZE        PIC 9(3) VALUE 10.
       01 I                 PIC 9(3).
       01 J                 PIC 9(3).
       01 KEY               PIC 9(3).
       01 TEMP              PIC 9(3).
       01 ARRAY-TABLE.
          05 ARRAY-ITEMS    OCCURS 10 TIMES PIC 9(3).

       01 DISPLAY-ARRAY.
          05 DISPLAY-ITEM   OCCURS 10 TIMES PIC Z(3) VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM INITIALIZE-ARRAY
           PERFORM DISPLAY-ARRAY-VALUES
           PERFORM INSERTION-SORT-ALGORITHM
           PERFORM DISPLAY-SORTED-ARRAY
           STOP RUN.

       INITIALIZE-ARRAY.
           MOVE 64 TO ARRAY-ITEMS(1)
           MOVE 34 TO ARRAY-ITEMS(2)
           MOVE 25 TO ARRAY-ITEMS(3)
           MOVE 12 TO ARRAY-ITEMS(4)
           MOVE 22 TO ARRAY-ITEMS(5)
           MOVE 11 TO ARRAY-ITEMS(6)
           MOVE 90 TO ARRAY-ITEMS(7)
           MOVE 88 TO ARRAY-ITEMS(8)
           MOVE 76 TO ARRAY-ITEMS(9)
           MOVE 50 TO ARRAY-ITEMS(10).

       DISPLAY-ARRAY-VALUES.
           DISPLAY "ORIGINAL ARRAY:"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               MOVE ARRAY-ITEMS(I) TO DISPLAY-ITEM(I)
           END-PERFORM
           DISPLAY FUNCTION REDUCING(DISPLAY-ITEM(1) DISPLAY-ITEM(2)
                                   DISPLAY-ITEM(3) DISPLAY-ITEM(4)
                                   DISPLAY-ITEM(5) DISPLAY-ITEM(6)
                                   DISPLAY-ITEM(7) DISPLAY-ITEM(8)
                                   DISPLAY-ITEM(9) DISPLAY-ITEM(10)).

       INSERTION-SORT-ALGORITHM.
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > ARRAY-SIZE
               MOVE ARRAY-ITEMS(I) TO KEY
               MOVE I TO J
               
               PERFORM UNTIL J = 1 OR ARRAY-ITEMS(J-1) <= KEY
                   MOVE ARRAY-ITEMS(J-1) TO ARRAY-ITEMS(J)
                   SUBTRACT 1 FROM J
               END-PERFORM
               
               MOVE KEY TO ARRAY-ITEMS(J)
           END-PERFORM.

       DISPLAY-SORTED-ARRAY.
           DISPLAY "SORTED ARRAY:"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               MOVE ARRAY-ITEMS(I) TO DISPLAY-ITEM(I)
           END-PERFORM
           DISPLAY FUNCTION REDUCING(DISPLAY-ITEM(1) DISPLAY-ITEM(2)
                                   DISPLAY-ITEM(3) DISPLAY-ITEM(4)
                                   DISPLAY-ITEM(5) DISPLAY-ITEM(6)
                                   DISPLAY-ITEM(7) DISPLAY-ITEM(8)
                                   DISPLAY-ITEM(9) DISPLAY-ITEM(10)).
```

## How the Insertion Sort Works in COBOL:

1. **Initialization**: The array is filled with sample data values
2. **Outer Loop**: Starting from the second element (index 2), each element is considered as a "key"
3. **Inner Loop**: The key is compared with elements to its left, shifting larger elements one position to the right
4. **Insertion**: The key is placed in its correct position
5. **Repeat**: Continue until all elements are processed

## Sample Output:
```
ORIGINAL ARRAY:
  64  34  25  12  22  11  90  88  76  50
SORTED ARRAY:
  11  12  22  25  34  50  64  76  88  90
```

This COBOL implementation demonstrates the classic insertion sort algorithm with proper data handling and structured programming techniques suitable for mainframe environments.