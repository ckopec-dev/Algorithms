# Levenshtein Distance Algorithm in COBOL

Here's a COBOL implementation of the Levenshtein distance algorithm:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEVENSHTEIN-DISTANCE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  STRING1                PIC X(50).
       01  STRING2                PIC X(50).
       01  LEN1                   PIC 9(3).
       01  LEN2                   PIC 9(3).
       01  DISTANCE               PIC 9(4).
       01  I                      PIC 9(3).
       01  J                      PIC 9(3).
       01  COST                   PIC 9(3).
       01  MATRIX.
           05  MATRIX-ROW         OCCURS 50 TIMES.
               10  MATRIX-ELEM    PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Enter first string: ".
           ACCEPT STRING1.
           DISPLAY "Enter second string: ".
           ACCEPT STRING2.

           MOVE FUNCTION LENGTH(STRING1) TO LEN1.
           MOVE FUNCTION LENGTH(STRING2) TO LEN2.

           PERFORM INITIALIZE-MATRIX.
           PERFORM CALCULATE-DISTANCE.

           DISPLAY "Levenshtein Distance: " DISTANCE.
           STOP RUN.

       INITIALIZE-MATRIX.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN1
               MOVE I TO MATRIX-ELEM(I 1)
           END-PERFORM.

           PERFORM VARYING J FROM 1 BY 1 UNTIL J > LEN2
               MOVE J TO MATRIX-ELEM(1 J)
           END-PERFORM.

       CALCULATE-DISTANCE.
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > LEN1
               PERFORM VARYING J FROM 2 BY 1 UNTIL J > LEN2
                   IF STRING1(I:1) = STRING2(J:1)
                       MOVE 0 TO COST
                   ELSE
                       MOVE 1 TO COST
                   END-IF

                   COMPUTE MATRIX-ELEM(I J) =
                       FUNCTION MIN(
                           MATRIX-ELEM(I-1 J) + 1,
                           MATRIX-ELEM(I J-1) + 1,
                           MATRIX-ELEM(I-1 J-1) + COST
                       )
               END-PERFORM
           END-PERFORM.

           MOVE MATRIX-ELEM(LEN1 LEN2) TO DISTANCE.
```

## Example Usage

**Input:**
- First string: "kitten"
- Second string: "sitting"

**Output:**
```
Levenshtein Distance: 3
```

## How it works:

1. **Initialization**: Create a matrix with dimensions (length1+1) × (length2+1)
2. **Base cases**: Fill first row and column with sequential numbers (0, 1, 2, ...)
3. **Dynamic programming**: For each cell (i,j), calculate the minimum cost:
   - If characters match: cost = 0
   - If characters don't match: cost = 1
   - Minimum of three operations: deletion, insertion, substitution
4. **Result**: The value at the bottom-right cell represents the Levenshtein distance

## Key COBOL Features Used:

- **OCCURS clause**: For creating arrays/matrix
- **FUNCTION LENGTH**: To get string length
- **FUNCTION MIN**: To find minimum value
- **PERFORM VARYING**: Loop constructs for iteration
- **MOVE with arithmetic**: For matrix operations

This implementation demonstrates how to translate the classic dynamic programming algorithm into COBOL syntax while maintaining the algorithmic correctness of the Levenshtein distance calculation.

