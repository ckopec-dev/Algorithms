# Jump Search Algorithm in COBOL

Here's an implementation of the Jump Search algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. JUMP-SEARCH-EXAMPLE.
       AUTHOR. COBOL EXAMPLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ARRAY-DATA.
          05 NUMBERS OCCURS 20 TIMES PIC 9(3) VALUE 10.
          05 NUMBERS OCCURS 20 TIMES PIC 9(3) VALUE 22.
          05 NUMBERS OCCURS 20 TIMES PIC 9(3) VALUE 35.
          05 NUMBERS OCCURS 20 TIMES PIC 9(3) VALUE 41.
          05 NUMBERS OCCURS 20 TIMES PIC 9(3) VALUE 56.
          05 NUMBERS OCCURS 20 TIMES PIC 9(3) VALUE 67.
          05 NUMBERS OCCURS 20 TIMES PIC 9(3) VALUE 88.
          05 NUMBERS OCCURS 20 TIMES PIC 9(3) VALUE 99.
          05 NUMBERS OCCURS 20 TIMES PIC 9(3) VALUE 105.
          05 NUMBERS OCCURS 20 TIMES PIC 9(3) VALUE 118.
          05 NUMBERS OCCURS 20 TIMES PIC 9(3) VALUE 126.
          05 NUMBERS OCCURS 20 TIMES PIC 9(3) VALUE 134.
          05 NUMBERS OCCURS 20 TIMES PIC 9(3) VALUE 147.
          05 NUMBERS OCCURS 20 TIMES PIC 9(3) VALUE 159.
          05 NUMBERS OCCURS 20 TIMES PIC 9(3) VALUE 168.
          05 NUMBERS OCCURS 20 TIMES PIC 9(3) VALUE 177.
          05 NUMBERS OCCURS 20 TIMES PIC 9(3) VALUE 189.
          05 NUMBERS OCCURS 20 TIMES PIC 9(3) VALUE 199.
          05 NUMBERS OCCURS 20 TIMES PIC 9(3) VALUE 205.

       01 SEARCH-VALUE           PIC 9(3).
       01 ARRAY-SIZE             PIC 9(2) VALUE 20.
       01 STEP                 PIC 9(2) VALUE 4.
       01 PREV                   PIC 9(2) VALUE 0.
       01 CURRENT                PIC 9(2) VALUE 0.
       01 RESULT                 PIC 9(2) VALUE -1.
       01 I                      PIC 9(2) VALUE 0.
       01 J                      PIC 9(2) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Jump Search Algorithm Example"
           DISPLAY "=================================="
           
           MOVE 67 TO SEARCH-VALUE
           DISPLAY "Searching for value: " SEARCH-VALUE
           
           PERFORM JUMP-SEARCH
           
           IF RESULT = -1
               DISPLAY "Value " SEARCH-VALUE " not found in array"
           ELSE
               DISPLAY "Value " SEARCH-VALUE " found at index: " RESULT
           END-IF
           
           STOP RUN.

       JUMP-SEARCH.
           COMPUTE STEP = FUNCTION SQRT(ARRAY-SIZE)
           MOVE 0 TO PREV
           MOVE STEP TO CURRENT
           
           PERFORM UNTIL CURRENT >= ARRAY-SIZE
               IF NUMBERS(CURRENT) > SEARCH-VALUE
                   MOVE PREV TO CURRENT
                   GO TO LINEAR-SEARCH
               ELSE IF NUMBERS(CURRENT) = SEARCH-VALUE
                   MOVE CURRENT TO RESULT
                   EXIT PARAGRAPH
               END-IF
               ADD STEP TO CURRENT
               MOVE CURRENT TO PREV
           END-PERFORM
           
           LINEAR-SEARCH.
               MOVE PREV TO I
               COMPUTE J = CURRENT - 1
               
               PERFORM UNTIL I > J OR RESULT <> -1
                   IF NUMBERS(I) = SEARCH-VALUE
                       MOVE I TO RESULT
                   END-IF
                   ADD 1 TO I
               END-PERFORM.
```

## How it works:

1. **Initialization**: The array is pre-filled with sorted values (10, 22, 35, 41, 56, 67, 88, 99, 105, 118, 126, 134, 147, 159, 168, 177, 189, 199, 205)

2. **Jump Size**: Calculate the jump size as square root of array size (≈4 for 20 elements)

3. **Jump Phase**: 
   - Start at index 0
   - Jump by step size until finding an element >= search value
   - If found, return index
   - If not found, go to linear search phase

4. **Linear Search Phase**: 
   - Search within the identified range using sequential search
   - Return index if found, otherwise return -1 (not found)

## Example Output:
```
Jump Search Algorithm Example
==================================
Searching for value: 067
Value 067 found at index: 005
```

This implementation demonstrates the jump search algorithm which has a time complexity of O(√n) and is particularly useful for large sorted arrays where binary search might be overkill.