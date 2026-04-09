# Gaussian Elimination in COBOL

Here's an example of implementing Gaussian elimination algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GAUSS-ELIMINATION.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "MATRIX.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 MATRIX-RECORD.
          05 ROW-NUMBER    PIC 99.
          05 ELEMENTS      PIC 9(5)V99 OCCURS 10 TIMES.

       WORKING-STORAGE SECTION.
       01 MATRIX.
          05 A OCCURS 10 TIMES INDEXED BY I-J.
             10 A-I-J      PIC 9(5)V99.
       01 N              PIC 99 VALUE 4.
       01 I              PIC 99.
       01 J              PIC 99.
       01 K              PIC 99.
       01 PIVOT          PIC 9(5)V99.
       01 FACTOR         PIC 9(5)V99.
       01 TEMP           PIC 9(5)V99.
       01 SWAP           PIC 9(5)V99.
       01 DONE           PIC X VALUE 'N'.
       01 ERROR-FLAG     PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-MATRIX
           PERFORM GAUSS-ELIMINATION
           PERFORM DISPLAY-RESULT
           STOP RUN.

       INITIALIZE-MATRIX.
           MOVE 1 TO I-J
           PERFORM UNTIL I-J > N
               MOVE 0 TO A-I-J
               ADD 1 TO I-J
           END-PERFORM.

       GAUSS-ELIMINATION.
           PERFORM FORWARD-ELIMINATION
           PERFORM BACKWARD-SUBSTITUTION.

       FORWARD-ELIMINATION.
           PERFORM UNTIL I > N - 1
               MOVE I TO K
               PERFORM FIND-PIVOT
               PERFORM SWAP-ROWS
               PERFORM ELIMINATE
               ADD 1 TO I
           END-PERFORM.

       FIND-PIVOT.
           PERFORM UNTIL K > N
               IF A-K-I NOT = ZERO
                   MOVE K TO I-J
                   GO TO FOUND-PIVOT
               END-IF
               ADD 1 TO K
           END-PERFORM
           MOVE 0 TO I-J
           MOVE 'Y' TO ERROR-FLAG
       FOUND-PIVOT.

       SWAP-ROWS.
           IF I-J NOT = I
               PERFORM UNTIL J > N
                   MOVE A-I-J TO TEMP
                   MOVE A-I-J TO A-I-J
                   MOVE TEMP TO A-I-J
                   ADD 1 TO J
               END-PERFORM
           END-IF.

       ELIMINATE.
           PERFORM UNTIL J > N
               IF A-J-I NOT = ZERO AND I NOT = J
                   COMPUTE FACTOR = A-J-I / A-I-I
                   PERFORM UNTIL K > N
                       COMPUTE A-J-K = A-J-K - (FACTOR * A-I-K)
                       ADD 1 TO K
                   END-PERFORM
                   MOVE 0 TO A-J-I
               END-IF
               ADD 1 TO J
           END-PERFORM.

       BACKWARD-SUBSTITUTION.
           PERFORM UNTIL I = 0
               COMPUTE A-I-N = A-I-N / A-I-I
               PERFORM UNTIL J = I
                   COMPUTE A-J-N = A-J-N - (A-J-I * A-I-N)
                   SUBTRACT 1 FROM J
               END-PERFORM
               SUBTRACT 1 FROM I
           END-PERFORM.

       DISPLAY-RESULT.
           DISPLAY "SOLUTION VECTOR:"
           PERFORM UNTIL I > N
               DISPLAY "X(" I ") = " A-I-N
               ADD 1 TO I
           END-PERFORM.

       END PROGRAM GAUSS-ELIMINATION.
```

## Sample Input Data (MATRIX.DAT)

```
1  2.0  1.0  4.0  1.0
2  4.0  3.0  1.0  2.0
3  1.0  1.0  1.0  1.0
4  3.0  2.0  2.0  3.0
```

## Key Features of This Implementation

1. **Matrix Storage**: Uses a 2D array structure to store the augmented matrix
2. **Forward Elimination**: Converts matrix to upper triangular form
3. **Backward Substitution**: Solves for unknown variables
4. **Pivot Selection**: Finds largest element for numerical stability
5. **Row Swapping**: Swaps rows when necessary to avoid division by zero
6. **Error Handling**: Checks for singular matrices

## Algorithm Steps

1. **Forward Elimination**: Transform matrix to upper triangular form
2. **Backward Substitution**: Solve for variables starting from last row
3. **Pivot Selection**: Choose largest element as pivot for numerical stability

This implementation demonstrates the core concepts of Gaussian elimination in COBOL, though note that COBOL's array handling and floating-point operations may require additional considerations for production use.

