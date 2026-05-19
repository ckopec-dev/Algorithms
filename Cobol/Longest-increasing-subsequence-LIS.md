# Longest Increasing Subsequence (LIS) in COBOL

Here's a COBOL implementation of the Longest Increasing Subsequence algorithm using dynamic programming:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIS-PROGRAM.
       AUTHOR. COBOL-EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  ARRAY-SIZE         PIC 9(3) VALUE 8.
       01  I                  PIC 9(3).
       01  J                  PIC 9(3).
       01  MAX-LENGTH         PIC 9(3).
       01  TEMP-LENGTH        PIC 9(3).
       01  RESULT             PIC 9(3).

       01  INPUT-ARRAY.
          05  ARRAY-ITEM         PIC 9(3) OCCURS 10 TIMES.

       01  LIS-ARRAY.
          05  LIS-ITEM           PIC 9(3) OCCURS 10 TIMES.

       01  DISPLAY-OUTPUT.
          05  DISPLAY-TEXT       PIC X(50) VALUE 
             "LONGEST INCREASING SUBSEQUENCE RESULTS".
          05  FILLER             PIC X(10) VALUE SPACE.
          05  DISPLAY-RESULT     PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "LONGEST INCREASING SUBSEQUENCE ALGORITHM"
           DISPLAY "================================================"
           
           * Initialize input array
           MOVE 10 TO ARRAY-ITEM(1)
           MOVE 22 TO ARRAY-ITEM(2)
           MOVE 9 TO ARRAY-ITEM(3)
           MOVE 33 TO ARRAY-ITEM(4)
           MOVE 21 TO ARRAY-ITEM(5)
           MOVE 50 TO ARRAY-ITEM(6)
           MOVE 41 TO ARRAY-ITEM(7)
           MOVE 60 TO ARRAY-ITEM(8)
           
           DISPLAY "INPUT ARRAY:"
           PERFORM DISPLAY-ARRAY
           
           * Call LIS function
           PERFORM CALCULATE-LIS
           
           DISPLAY "LENGTH OF LIS: " RESULT
           DISPLAY "================================================"
           
           STOP RUN.

       DISPLAY-ARRAY.
           DISPLAY "Array elements: "
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               DISPLAY ARRAY-ITEM(I) WITH NO ADVANCING
           END-PERFORM
           DISPLAY "."

       CALCULATE-LIS.
           * Initialize LIS array with 1 for each element
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               MOVE 1 TO LIS-ITEM(I)
           END-PERFORM
           
           * Fill LIS array using dynamic programming
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > ARRAY-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J >= I
                   IF ARRAY-ITEM(J) < ARRAY-ITEM(I)
                       COMPUTE TEMP-LENGTH = LIS-ITEM(J) + 1
                       IF TEMP-LENGTH > LIS-ITEM(I)
                           MOVE TEMP-LENGTH TO LIS-ITEM(I)
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
           
           * Find maximum length
           MOVE 1 TO MAX-LENGTH
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               IF LIS-ITEM(I) > MAX-LENGTH
                   MOVE LIS-ITEM(I) TO MAX-LENGTH
               END-IF
           END-PERFORM
           
           MOVE MAX-LENGTH TO RESULT.

       END PROGRAM LIS-PROGRAM.
```

## Algorithm Explanation

This COBOL implementation demonstrates the **Longest Increasing Subsequence (LIS)** algorithm using dynamic programming:

### Key Components:

1. **Input Array**: Contains the sequence [10, 22, 9, 33, 21, 50, 41, 60]
2. **LIS Array**: Stores the length of LIS ending at each position
3. **Dynamic Programming**: For each element, we check all previous elements
4. **Optimization**: We only update LIS values when we find a longer increasing subsequence

### Algorithm Steps:

1. **Initialization**: Create LIS array with all values set to 1
2. **DP Computation**: For each element, check all previous elements
3. **Comparison**: If current element is greater than previous element:
   - Update LIS value if we found a longer subsequence
4. **Result**: Find maximum value in LIS array

### Expected Output:
```
LONGEST INCREASING SUBSEQUENCE ALGORITHM
================================================
INPUT ARRAY:
Array elements: 10 22 9 33 21 50 41 60 .
LENGTH OF LIS: 5
================================================
```

The LIS for the sequence [10, 22, 9, 33, 21, 50, 41, 60] is [10, 22, 33, 50, 60] with length 5.

### Time Complexity: O(n²)
### Space Complexity: O(n)

This implementation shows how classic algorithms can be adapted for COBOL's structured programming approach while maintaining the core dynamic programming logic.

