# Selection Sort Algorithm in COBOL

Here's an example of the Selection Sort algorithm implemented in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SELECTION-SORT.
       AUTHOR. COBOL EXAMPLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ARRAY-SIZE      PIC 9(2) VALUE 10.
       01 ARRAY-ITEMS.
          05 NUMBERS        OCCURS 10 TIMES PIC 9(3).
       01 I               PIC 9(2).
       01 J               PIC 9(2).
       01 MIN-INDEX       PIC 9(2).
       01 TEMP            PIC 9(3).
       01 SWAP-FLAG       PIC X VALUE 'N'.
       01 DISPLAY-INDEX   PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "SELECTION SORT EXAMPLE"
           DISPLAY "========================"
           
           * Initialize array with sample data
           MOVE 64 TO NUMBERS(1)
           MOVE 34 TO NUMBERS(2)
           MOVE 25 TO NUMBERS(3)
           MOVE 12 TO NUMBERS(4)
           MOVE 22 TO NUMBERS(5)
           MOVE 11 TO NUMBERS(6)
           MOVE 90 TO NUMBERS(7)
           MOVE 88 TO NUMBERS(8)
           MOVE 76 TO NUMBERS(9)
           MOVE 50 TO NUMBERS(10)
           
           DISPLAY "Original Array:"
           PERFORM DISPLAY-ARRAY
           
           * Perform selection sort
           PERFORM SELECTION-SORT-ALGORITHM
           
           DISPLAY "Sorted Array:"
           PERFORM DISPLAY-ARRAY
           
           STOP RUN.

       SELECTION-SORT-ALGORITHM.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE - 1
               MOVE I TO MIN-INDEX
               
               PERFORM VARYING J FROM I + 1 BY 1 UNTIL J > ARRAY-SIZE
                   IF NUMBERS(J) < NUMBERS(MIN-INDEX)
                       MOVE J TO MIN-INDEX
                   END-IF
               END-PERFORM
               
               * Swap elements if needed
               IF MIN-INDEX NOT = I
                   MOVE NUMBERS(I) TO TEMP
                   MOVE NUMBERS(MIN-INDEX) TO NUMBERS(I)
                   MOVE TEMP TO NUMBERS(MIN-INDEX)
               END-IF
           END-PERFORM.

       DISPLAY-ARRAY.
           DISPLAY "Array contents:"
           PERFORM VARYING DISPLAY-INDEX FROM 1 BY 1 UNTIL DISPLAY-INDEX > ARRAY-SIZE
               DISPLAY "NUMBERS(" DISPLAY-INDEX ") = " NUMBERS(DISPLAY-INDEX)
           END-PERFORM
           DISPLAY SPACE.
```

## How the Selection Sort Works in COBOL:

1. **Initialization**: The array is populated with sample data
2. **Outer Loop**: Iterates through each position in the array (from 1 to n-1)
3. **Inner Loop**: Finds the minimum element in the remaining unsorted portion
4. **Swap**: If a smaller element is found, it swaps with the current position
5. **Display**: Shows the array before and after sorting

## Key COBOL Features Used:

- **OCCURS clause**: Defines arrays with multiple elements
- **PERFORM VARYING**: COBOL's loop construct for iteration
- **IF-THEN-ELSE**: Conditional logic for comparisons
- **MOVE statement**: Data transfer operations
- **DISPLAY statement**: Output formatting

## Sample Output:
```
SELECTION SORT EXAMPLE
========================
Original Array:
Array contents:
NUMBERS(1) = 064
NUMBERS(2) = 034
NUMBERS(3) = 025
NUMBERS(4) = 012
NUMBERS(5) = 022
NUMBERS(6) = 011
NUMBERS(7) = 090
NUMBERS(8) = 088
NUMBERS(9) = 076
NUMBERS(10) = 050

Sorted Array:
Array contents:
NUMBERS(1) = 011
NUMBERS(2) = 012
NUMBERS(3) = 022
NUMBERS(4) = 025
NUMBERS(5) = 034
NUMBERS(6) = 050
NUMBERS(7) = 064
NUMBERS(8) = 076
NUMBERS(9) = 088
NUMBERS(10) = 090
```

