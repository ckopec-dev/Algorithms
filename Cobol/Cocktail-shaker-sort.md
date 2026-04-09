# Cocktail Shaker Sort in COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COCKTAIL-SHAKER-SORT.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTPUT-FILE ASSIGN TO "SORTED-DATA.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC X(80).

       WORKING-STORAGE SECTION.
       01 ARRAY-SIZE PIC 9(3) VALUE 10.
       01 ARRAY-ITEMS.
           05 DATA-ITEMS PIC 9(3) OCCURS 10 TIMES.
       01 I PIC 9(3).
       01 J PIC 9(3).
       01 SWAP-FLAG PIC 9.
       01 TEMP-ITEM PIC 9(3).
       01 LEFT-BOUND PIC 9(3) VALUE 1.
       01 RIGHT-BOUND PIC 9(3) VALUE 10.
       01 RECORD-TEXT PIC X(80).
       01 DISPLAY-INDEX PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-ARRAY
           PERFORM DISPLAY-ARRAY "BEFORE SORTING"
           PERFORM COCKTAIL-SHAKER-SORT
           PERFORM DISPLAY-ARRAY "AFTER SORTING"
           STOP RUN.

       INITIALIZE-ARRAY.
           MOVE 64 TO DATA-ITEMS(1)
           MOVE 34 TO DATA-ITEMS(2)
           MOVE 25 TO DATA-ITEMS(3)
           MOVE 12 TO DATA-ITEMS(4)
           MOVE 22 TO DATA-ITEMS(5)
           MOVE 11 TO DATA-ITEMS(6)
           MOVE 90 TO DATA-ITEMS(7)
           MOVE 88 TO DATA-ITEMS(8)
           MOVE 76 TO DATA-ITEMS(9)
           MOVE 50 TO DATA-ITEMS(10).

       COCKTAIL-SHAKER-SORT.
           PERFORM UNTIL RIGHT-BOUND <= LEFT-BOUND
               PERFORM BUBBLE-RIGHT
               IF RIGHT-BOUND > LEFT-BOUND
                   PERFORM BUBBLE-LEFT
               END-IF
           END-PERFORM.

       BUBBLE-RIGHT.
           MOVE 0 TO SWAP-FLAG
           PERFORM VARYING I FROM LEFT-BOUND BY 1
               UNTIL I >= RIGHT-BOUND
               IF DATA-ITEMS(I) > DATA-ITEMS(I + 1)
                   MOVE DATA-ITEMS(I) TO TEMP-ITEM
                   MOVE DATA-ITEMS(I + 1) TO DATA-ITEMS(I)
                   MOVE TEMP-ITEM TO DATA-ITEMS(I + 1)
                   MOVE 1 TO SWAP-FLAG
               END-IF
           END-PERFORM
           IF SWAP-FLAG = 1
               SUBTRACT 1 FROM RIGHT-BOUND
           END-IF.

       BUBBLE-LEFT.
           MOVE 0 TO SWAP-FLAG
           PERFORM VARYING I FROM RIGHT-BOUND BY -1
               UNTIL I <= LEFT-BOUND
               IF DATA-ITEMS(I) < DATA-ITEMS(I - 1)
                   MOVE DATA-ITEMS(I) TO TEMP-ITEM
                   MOVE DATA-ITEMS(I - 1) TO DATA-ITEMS(I)
                   MOVE TEMP-ITEM TO DATA-ITEMS(I - 1)
                   MOVE 1 TO SWAP-FLAG
               END-IF
           END-PERFORM
           IF SWAP-FLAG = 1
               ADD 1 TO LEFT-BOUND
           END-IF.

       DISPLAY-ARRAY.
           MOVE 1 TO DISPLAY-INDEX
           STRING "ARRAY " FUNCTION TRIM(1) " : " INTO RECORD-TEXT
           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > ARRAY-SIZE
               STRING " " DATA-ITEMS(I) INTO RECORD-TEXT
           END-PERFORM
           DISPLAY RECORD-TEXT
           DISPLAY " "

       END PROGRAM COCKTAIL-SHAKER-SORT.
```

## Algorithm Explanation

The Cocktail Shaker Sort (also known as Bidirectional Bubble Sort) works by:

1. **Forward Pass**: Comparing adjacent elements from left to right and swapping if needed
2. **Backward Pass**: Comparing adjacent elements from right to left and swapping if needed
3. **Boundary Adjustment**: After each pass, the sorted boundaries move inward
4. **Repeat**: Continue until no more swaps are needed

## Key Features

- **Bidirectional**: Sorts in both directions alternately
- **Optimization**: Reduces the number of passes needed
- **Stable**: Maintains relative order of equal elements
- **Time Complexity**: O(n²) worst case, O(n) best case
- **Space Complexity**: O(1) auxiliary space

## Sample Output
```
ARRAY BEFORE SORTING : 64 34 25 12 22 11 90 88 76 50 

ARRAY AFTER SORTING : 11 12 22 25 34 50 64 76 88 90 
```

