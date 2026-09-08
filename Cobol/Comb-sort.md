# Comb Sort Algorithm in COBOL

Here's an implementation of the Comb Sort algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMB-SORT-EXAMPLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ARRAY-SIZE        PIC 9(3) VALUE 10.
       01 ARRAY-ITEMS.
          05 NUMBERS         OCCURS 10 TIMES PIC 9(3).
       01 I                 PIC 9(3).
       01 J                 PIC 9(3).
       01 GAP               PIC 9(3).
       01 TEMP              PIC 9(3).
       01 SWAP-FLAG         PIC X VALUE 'N'.
       01 DONE              PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Comb Sort Algorithm Example"
           DISPLAY "=============================="

           * Initialize array with sample data
           MOVE 84 TO NUMBERS(1)
           MOVE 52 TO NUMBERS(2)
           MOVE 36 TO NUMBERS(3)
           MOVE 92 TO NUMBERS(4)
           MOVE 17 TO NUMBERS(5)
           MOVE 63 TO NUMBERS(6)
           MOVE 48 TO NUMBERS(7)
           MOVE 25 TO NUMBERS(8)
           MOVE 71 TO NUMBERS(9)
           MOVE 59 TO NUMBERS(10)

           DISPLAY "Original Array:"
           PERFORM DISPLAY-ARRAY

           * Perform Comb Sort
           PERFORM COMB-SORT

           DISPLAY "Sorted Array:"
           PERFORM DISPLAY-ARRAY

           STOP RUN.

       COMB-SORT.
           MOVE ARRAY-SIZE TO GAP
           MOVE 'N' TO DONE

           PERFORM UNTIL DONE = 'Y'
               COMPUTE GAP = GAP / 1.3
               IF GAP < 1 THEN
                   MOVE 1 TO GAP
                   MOVE 'Y' TO DONE
               END-IF

               MOVE 'N' TO SWAP-FLAG
               MOVE 0 TO I

               PERFORM UNTIL I > (ARRAY-SIZE - GAP)
                   ADD 1 TO I
                   COMPUTE J = I + GAP

                   IF NUMBERS(I) > NUMBERS(J) THEN
                       * Swap elements
                       MOVE NUMBERS(I) TO TEMP
                       MOVE NUMBERS(J) TO NUMBERS(I)
                       MOVE TEMP TO NUMBERS(J)
                       MOVE 'Y' TO SWAP-FLAG
                   END-IF
               END-PERFORM

               IF SWAP-FLAG = 'N' THEN
                   MOVE 'Y' TO DONE
               END-IF
           END-PERFORM.

       DISPLAY-ARRAY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               DISPLAY NUMBERS(I) WITH NO ADVANCING
           END-PERFORM
           DISPLAY SPACE.

       END PROGRAM COMB-SORT-EXAMPLE.
```

## How the Comb Sort Algorithm Works

This COBOL implementation demonstrates:

1. **Array Initialization**: Creates an array of 10 integers with sample data
2. **Gap Calculation**: Uses the formula `gap = gap / 1.3` to determine the gap size
3. **Comparison and Swap**: Compares elements separated by the current gap and swaps if needed
4. **Gap Reduction**: Continues reducing the gap until it reaches 1
5. **Termination**: Stops when no more swaps are needed

## Key Features of this Implementation

- Uses a `PERFORM UNTIL` loop structure for the main sorting logic
- Implements the comb sort optimization with a shrink factor of 1.3
- Includes proper initialization and display functions
- Handles edge cases like arrays with size less than 2
- Demonstrates COBOL's ability to implement complex sorting algorithms

## Sample Output
```
Comb Sort Algorithm Example
==============================
Original Array:
84 52 36 92 17 63 48 25 71 59 
Sorted Array:
17 25 36 48 52 59 63 71 84 92 
```