# Subset Sum Algorithm in COBOL

Here's an example of a Subset Sum algorithm implemented in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUBSET-SUM-ALGORITHM.
       AUTHOR. COBOL-PROGRAMMER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  WS-TARGET-SUM        PIC 9(5) VALUE 10.
       01  WS-ARRAY-SIZE        PIC 9(2) VALUE 5.
       01  WS-ARRAY.
           05  WS-ARRAY-ELEMENTS    OCCURS 10 TIMES PIC 9(3).
       01  WS-INDEX             PIC 9(2).
       01  WS-SUBSET-SUM        PIC 9(5).
       01  WS-FOUND             PIC X VALUE 'N'.
       01  WS-TEMP-SUM          PIC 9(5).
       01  WS-BIT-MASK          PIC 9(10).
       01  WS-RESULT            PIC X(100).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "SUBSET SUM ALGORITHM EXAMPLE"
           DISPLAY "============================"
           
           * Initialize array with sample values
           MOVE 3 TO WS-ARRAY-ELEMENTS(1)
           MOVE 5 TO WS-ARRAY-ELEMENTS(2)
           MOVE 7 TO WS-ARRAY-ELEMENTS(3)
           MOVE 1 TO WS-ARRAY-ELEMENTS(4)
           MOVE 2 TO WS-ARRAY-ELEMENTS(5)
           
           DISPLAY "Array elements: "
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > WS-ARRAY-SIZE
               DISPLAY WS-ARRAY-ELEMENTS(WS-INDEX) " "
           END-PERFORM
           DISPLAY ""
           DISPLAY "Target sum: " WS-TARGET-SUM
           DISPLAY ""
           
           * Call subset sum function
           PERFORM SUBSET-SUM-FUNCTION
           
           IF WS-FOUND = 'Y'
               DISPLAY "SUBSET FOUND!"
               DISPLAY "A subset that sums to " WS-TARGET-SUM " exists"
           ELSE
               DISPLAY "NO SUBSET FOUND!"
               DISPLAY "No subset sums to " WS-TARGET-SUM
           END-IF
           
           STOP RUN.

       SUBSET-SUM-FUNCTION.
           * Using bit manipulation approach to check all possible subsets
           MOVE 0 TO WS-BIT-MASK
           MOVE 0 TO WS-SUBSET-SUM
           MOVE 'N' TO WS-FOUND
           
           PERFORM VARYING WS-BIT-MASK FROM 1 BY 1 UNTIL WS-BIT-MASK >= (2 ** WS-ARRAY-SIZE)
               MOVE 0 TO WS-SUBSET-SUM
               MOVE 0 TO WS-INDEX
               
               PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > WS-ARRAY-SIZE
                   COMPUTE WS-TEMP-SUM = 2 ** (WS-INDEX - 1)
                   IF WS-BIT-MASK AND WS-TEMP-SUM NOT = 0
                       ADD WS-ARRAY-ELEMENTS(WS-INDEX) TO WS-SUBSET-SUM
                   END-IF
               END-PERFORM
               
               IF WS-SUBSET-SUM = WS-TARGET-SUM
                   MOVE 'Y' TO WS-FOUND
                   EXIT PERFORM
               END-IF
           END-PERFORM.
```

## Alternative Implementation Using Recursive Approach

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUBSET-SUM-RECURSIVE.
       AUTHOR. COBOL-PROGRAMMER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TARGET-SUM        PIC 9(5) VALUE 10.
       01  WS-ARRAY-SIZE        PIC 9(2) VALUE 5.
       01  WS-ARRAY.
           05  WS-ARRAY-ELEMENTS    OCCURS 10 TIMES PIC 9(3).
       01  WS-INDEX             PIC 9(2).
       01  WS-RESULT            PIC X VALUE 'N'.
       01  WS-SUM               PIC 9(5).
       01  WS-TEMP-SUM          PIC 9(5).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "RECURSIVE SUBSET SUM ALGORITHM"
           DISPLAY "=============================="
           
           * Initialize array
           MOVE 3 TO WS-ARRAY-ELEMENTS(1)
           MOVE 5 TO WS-ARRAY-ELEMENTS(2)
           MOVE 7 TO WS-ARRAY-ELEMENTS(3)
           MOVE 1 TO WS-ARRAY-ELEMENTS(4)
           MOVE 2 TO WS-ARRAY-ELEMENTS(5)
           
           DISPLAY "Array elements: "
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > WS-ARRAY-SIZE
               DISPLAY WS-ARRAY-ELEMENTS(WS-INDEX) " "
           END-PERFORM
           DISPLAY ""
           DISPLAY "Target sum: " WS-TARGET-SUM
           DISPLAY ""
           
           * Call recursive function
           MOVE 0 TO WS-SUM
           PERFORM SUBSET-SUM-RECURSIVE WITH PARAMETER WS-ARRAY-SIZE WS-SUM
           
           IF WS-RESULT = 'Y'
               DISPLAY "SUBSET FOUND!"
           ELSE
               DISPLAY "NO SUBSET FOUND!"
           END-IF
           
           STOP RUN.

       SUBSET-SUM-RECURSIVE.
           01  P-INDEX              PIC 9(2).
           01  P-SUM                PIC 9(5).
           01  WS-TEMP              PIC 9(5).
           
           IF P-SUM = WS-TARGET-SUM
               MOVE 'Y' TO WS-RESULT
               EXIT PARAGRAPH
           END-IF
           
           IF P-INDEX > WS-ARRAY-SIZE OR P-SUM > WS-TARGET-SUM
               EXIT PARAGRAPH
           END-IF
           
           * Include current element
           COMPUTE WS-TEMP = P-SUM + WS-ARRAY-ELEMENTS(P-INDEX)
           PERFORM SUBSET-SUM-RECURSIVE WITH PARAMETER P-INDEX P-INDEX WS-TEMP
           
           * Exclude current element
           COMPUTE P-INDEX = P-INDEX + 1
           PERFORM SUBSET-SUM-RECURSIVE WITH PARAMETER P-INDEX P-INDEX P-SUM.
```

## Key Features of this Implementation:

1. **Array Initialization**: Sample array elements are set up for testing
2. **Bit Manipulation**: Uses bit masking to generate all possible subsets
3. **Sum Calculation**: Calculates sum for each subset combination
4. **Result Tracking**: Tracks whether a valid subset is found
5. **Output Display**: Shows the results clearly

## How it works:
- The algorithm generates all possible subsets using bit manipulation
- For each subset, it calculates the sum of elements
- If any subset sum equals the target sum, it returns success
- Time complexity: O(2^n) where n is the number of elements

This implementation demonstrates the classic subset sum problem solution in COBOL programming language.

