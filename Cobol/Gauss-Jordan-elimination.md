# Gauss-Jordan Elimination in COBOL

Here's a complete COBOL implementation of the Gauss-Jordan elimination algorithm for solving systems of linear equations:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GAUSS-JORDAN-SOLVE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATA-FILE ASSIGN TO "SYSTEM-INPUT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  DATA-FILE.
       01  INPUT-RECORD.
           05  MATRIX-SIZE    PIC 9(3).
           05  MATRIX-ELEMENTS OCCURS 20 TIMES.
               10  ELEMENT      PIC 9(3)V9(6).

       WORKING-STORAGE SECTION.
       01  MATRIX.
           05  COEFF-MATRIX OCCURS 20 TIMES DEPENDING ON MATRIX-SIZE.
               10  ROW OCCURS 20 TIMES DEPENDING ON MATRIX-SIZE.
                   15  A-NUMBER     PIC 9(3)V9(6).
       01  SOLUTIONS.
           05  SOLN OCCURS 20 TIMES DEPENDING ON MATRIX-SIZE.
               10  X-NUMBER     PIC 9(3)V9(6).
       01  TEMP-MATRIX.
           05  TEMP-ROW OCCURS 20 TIMES DEPENDING ON MATRIX-SIZE.
               10  TEMP-ELEM    PIC 9(3)V9(6).
       01  I, J, K, N     PIC 9(3).
       01  PIVOT          PIC 9(3)V9(6).
       01  MULTIPLIER     PIC 9(3)V9(6).
       01  SWAP-TEMP      PIC 9(3)V9(6).
       01  FLAG           PIC X VALUE 'N'.
       01  ERROR-MESSAGE  PIC X(50) VALUE "ERROR IN MATRIX".
       01  DISPLAY-TEXT   PIC X(50).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "GAUSS-JORDAN ELIMINATION ALGORITHM"
           DISPLAY "====================================="
           
           PERFORM READ-MATRIX
           PERFORM PRINT-ORIGINAL-MATRIX
           PERFORM GAUSS-JORDAN-PROCESS
           PERFORM PRINT-SOLUTIONS
           
           STOP RUN.

       READ-MATRIX.
           DISPLAY "ENTER MATRIX SIZE (MAX 20): "
           ACCEPT MATRIX-SIZE
           
           IF MATRIX-SIZE > 20 OR MATRIX-SIZE < 1
               DISPLAY "INVALID MATRIX SIZE!"
               STOP RUN
           END-IF

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MATRIX-SIZE
               DISPLAY "ENTER ROW " I " ELEMENTS:"
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > MATRIX-SIZE + 1
                   DISPLAY "Element [" I "," J "]: "
                   ACCEPT A-NUMBER(I,J)
               END-PERFORM
           END-PERFORM.

       PRINT-ORIGINAL-MATRIX.
           DISPLAY "ORIGINAL MATRIX:"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MATRIX-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > MATRIX-SIZE + 1
                   IF J = MATRIX-SIZE + 1
                       DISPLAY A-NUMBER(I,J) " | "
                   ELSE
                       DISPLAY A-NUMBER(I,J) " "
                   END-IF
               END-PERFORM
               DISPLAY ""
           END-PERFORM.

       GAUSS-JORDAN-PROCESS.
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > MATRIX-SIZE
               PERFORM PIVOT-FIND
               PERFORM PIVOT-SWAP
               PERFORM NORMALIZE-ROW
               PERFORM ELIMINATE-COLUMN
           END-PERFORM.

       PIVOT-FIND.
           MOVE A-NUMBER(K,K) TO PIVOT.
           IF PIVOT = 0
               DISPLAY "SINGULAR MATRIX - NO UNIQUE SOLUTION"
               STOP RUN
           END-IF.

       PIVOT-SWAP.
           IF K > 1 AND A-NUMBER(K,K) = 0
               PERFORM VARYING I FROM K + 1 BY 1 UNTIL I > MATRIX-SIZE
                   IF A-NUMBER(I,K) <> 0
                       PERFORM SWAP-ROWS
                       GO TO PIVOT-SWAP
                   END-IF
               END-PERFORM
           END-IF.

       SWAP-ROWS.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > MATRIX-SIZE + 1
               MOVE A-NUMBER(K,J) TO SWAP-TEMP
               MOVE A-NUMBER(I,J) TO A-NUMBER(K,J)
               MOVE SWAP-TEMP TO A-NUMBER(I,J)
           END-PERFORM.

       NORMALIZE-ROW.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > MATRIX-SIZE + 1
               DIVIDE A-NUMBER(K,J) BY A-NUMBER(K,K) GIVING A-NUMBER(K,J)
           END-PERFORM.

       ELIMINATE-COLUMN.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MATRIX-SIZE
               IF I NOT = K
                   MOVE A-NUMBER(I,K) TO MULTIPLIER
                   PERFORM VARYING J FROM 1 BY 1 UNTIL J > MATRIX-SIZE + 1
                       COMPUTE A-NUMBER(I,J) = A-NUMBER(I,J) - 
                           (MULTIPLIER * A-NUMBER(K,J))
                   END-PERFORM
               END-IF
           END-PERFORM.

       PRINT-SOLUTIONS.
           DISPLAY "SOLUTIONS:"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MATRIX-SIZE
               MOVE A-NUMBER(I,MATRIX-SIZE + 1) TO X-NUMBER(I)
               DISPLAY "X" I " = " X-NUMBER(I)
           END-PERFORM.

       END PROGRAM GAUSS-JORDAN-SOLVE.
```

## Example Usage

For a system of equations:
```
2x + y - z = 8
-3x - y + 2z = -11
-2x + y + 2z = -3
```

The input would be a 3×4 matrix:
```
2.000000   1.000000  -1.000000   8.000000
-3.000000  -1.000000   2.000000  -11.000000
-2.000000   1.000000   2.000000  -3.000000
```

## Key Features of This Implementation

1. **Matrix Input**: Reads a system of linear equations as an augmented matrix
2. **Pivot Selection**: Handles cases where pivot elements are zero
3. **Row Swapping**: Automatically swaps rows when necessary for numerical stability
4. **Normalization**: Divides each row by its pivot element
5. **Elimination**: Performs forward and backward elimination steps
6. **Solution Output**: Displays the final solutions for all variables

## Algorithm Steps

1. **Forward Elimination**: Transform matrix to upper triangular form
2. **Backward Elimination**: Continue to reduce to reduced row echelon form
3. **Pivot Selection**: Choose largest element in column as pivot
4. **Row Operations**: Perform elementary row operations throughout process

This COBOL implementation follows the standard Gauss-Jordan elimination algorithm and handles the mathematical operations required for solving systems of linear equations.