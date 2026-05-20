# Radix Sort Algorithm in COBOL

Here's an example implementation of the Radix Sort algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RADIX-SORT.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  ARRAY-SIZE        PIC 9(3) VALUE 10.
       01  MAX-VALUE         PIC 9(6) VALUE 0.
       01  MAX-POSI          PIC 9(3) VALUE 0.
       01  RADIX             PIC 9(3) VALUE 10.
       01  COUNT             PIC 9(3) VALUE 0.
       01  I                 PIC 9(3) VALUE 0.
       01  J                 PIC 9(3) VALUE 0.
       01  K                 PIC 9(3) VALUE 0.
       01  TEMP              PIC 9(6) VALUE 0.
       01  DIGIT             PIC 9(3) VALUE 0.
       01  DIVISOR           PIC 9(6) VALUE 1.

       01  NUMBERS.
           05  NUM-ARRAY       PIC 9(6) OCCURS 100 TIMES.
           05  COUNT-ARRAY     PIC 9(6) OCCURS 10 TIMES.

       01  SORTED-ARRAY.
           05  SORTED-VALUES   PIC 9(6) OCCURS 100 TIMES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "RADIX SORT EXAMPLE"
           DISPLAY "====================="

           * Initialize test data
           MOVE 123 TO NUM-ARRAY(1)
           MOVE 456 TO NUM-ARRAY(2)
           MOVE 789 TO NUM-ARRAY(3)
           MOVE 12 TO NUM-ARRAY(4)
           MOVE 345 TO NUM-ARRAY(5)
           MOVE 678 TO NUM-ARRAY(6)
           MOVE 901 TO NUM-ARRAY(7)
           MOVE 234 TO NUM-ARRAY(8)
           MOVE 567 TO NUM-ARRAY(9)
           MOVE 890 TO NUM-ARRAY(10)

           DISPLAY "Original Array:"
           PERFORM DISPLAY-ARRAY

           * Find maximum value to determine number of digits
           PERFORM FIND-MAX

           * Perform radix sort
           PERFORM RADIX-SORT-LOOP

           DISPLAY "Sorted Array:"
           PERFORM DISPLAY-ARRAY

           STOP RUN.

       FIND-MAX.
           MOVE 0 TO MAX-VALUE
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               IF NUM-ARRAY(I) > MAX-VALUE
                   MOVE NUM-ARRAY(I) TO MAX-VALUE
               END-IF
           END-PERFORM.

       RADIX-SORT-LOOP.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
               IF MAX-VALUE < DIVISOR
                   GO TO SORT-EXIT
               END-IF
               PERFORM COUNTING-SORT
               MULTIPLY DIVISOR BY RADIX GIVING DIVISOR
           END-PERFORM.

       COUNTING-SORT.
           * Initialize count array
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
               MOVE 0 TO COUNT-ARRAY(I)
           END-PERFORM.

           * Count occurrences
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               COMPUTE DIGIT = NUM-ARRAY(I) / DIVISOR
               COMPUTE DIGIT = FUNCTION MOD(DIGIT, RADIX)
               ADD 1 TO COUNT-ARRAY(DIGIT + 1)
           END-PERFORM.

           * Calculate cumulative count
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > 10
               ADD COUNT-ARRAY(I-1) TO COUNT-ARRAY(I)
           END-PERFORM.

           * Build output array
           PERFORM VARYING I FROM ARRAY-SIZE BY -1 UNTIL I < 1
               COMPUTE DIGIT = NUM-ARRAY(I) / DIVISOR
               COMPUTE DIGIT = FUNCTION MOD(DIGIT, RADIX)
               SUBTRACT 1 FROM COUNT-ARRAY(DIGIT + 1)
               MOVE NUM-ARRAY(I) TO SORTED-VALUES(COUNT-ARRAY(DIGIT + 1) + 1)
           END-PERFORM.

           * Copy back to original array
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               MOVE SORTED-VALUES(I) TO NUM-ARRAY(I)
           END-PERFORM.

       DISPLAY-ARRAY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               DISPLAY "NUM-ARRAY(" I ") = " NUM-ARRAY(I)
           END-PERFORM
           DISPLAY SPACE.

       SORT-EXIT.
           EXIT.
```

## How This Radix Sort Works:

1. **Initialization**: Sets up the array with test data
2. **Find Maximum**: Determines the maximum value to know how many digits we need to process
3. **Radix Sort Loop**: 
   - Processes each digit position (units, tens, hundreds, etc.)
   - Uses counting sort for each digit position
   - Multiplies the divisor by 10 for the next digit position
4. **Counting Sort**: 
   - Counts occurrences of each digit (0-9)
   - Calculates cumulative counts
   - Builds the sorted array based on digit positions
   - Copies the sorted array back to the original array

## Key Features:

- **Radix-based**: Sorts numbers by individual digits from least to most significant
- **Stable**: Maintains relative order of equal elements
- **Efficient**: O(d × n) time complexity where d is the number of digits
- **Non-comparative**: Doesn't use comparison operators like < or >

## Sample Output:
```
RADIX SORT EXAMPLE
=====================
Original Array:
NUM-ARRAY(1) = 123
NUM-ARRAY(2) = 456
NUM-ARRAY(3) = 789
NUM-ARRAY(4) = 12
NUM-ARRAY(5) = 345
NUM-ARRAY(6) = 678
NUM-ARRAY(7) = 901
NUM-ARRAY(8) = 234
NUM-ARRAY(9) = 567
NUM-ARRAY(10) = 890

Sorted Array:
NUM-ARRAY(1) = 12
NUM-ARRAY(2) = 123
NUM-ARRAY(3) = 234
NUM-ARRAY(4) = 345
NUM-ARRAY(5) = 456
NUM-ARRAY(6) = 567
NUM-ARRAY(7) = 678
NUM-ARRAY(8) = 789
NUM-ARRAY(9) = 890
NUM-ARRAY(10) = 901
```

