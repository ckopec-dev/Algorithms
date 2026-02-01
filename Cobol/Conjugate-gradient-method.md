# Conjugate Gradient Method in COBOL

Here's an example implementation of the Conjugate Gradient method in COBOL for solving linear systems Ax = b:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONJUGATE-GRADIENT-METHOD.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTPUT-FILE ASSIGN TO "CG-RESULTS.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
           05 OUTPUT-FIELD-1    PIC X(50).
           05 OUTPUT-FIELD-2    PIC X(20).
           05 OUTPUT-FIELD-3    PIC X(20).

       WORKING-STORAGE SECTION.
       01  MATRIX-LENGTH         PIC 9(3) VALUE 5.
       01  MAX-ITERATIONS        PIC 9(3) VALUE 100.
       01  TOLERANCE             PIC 9V999999 VALUE 0.0001.
       01  ITERATION-COUNT       PIC 9(3) VALUE 0.
       01  CONVERGED             PIC X VALUE "N".
       01  EPSILON               PIC 9V999999 VALUE 0.000001.

       01  A-MATRIX.
           05 A-ROW OCCURS 5 TIMES.
               10 A-ELEMENTS OCCURS 5 TIMES PIC 9V99.

       01  B-VECTOR.
           05 B-ELEMENT PIC 9V99 VALUE 0.

       01  X-VECTOR.
           05 X-ELEMENT PIC 9V99 VALUE 0.

       01  R-VECTOR.
           05 R-ELEMENT PIC 9V99 VALUE 0.

       01  P-VECTOR.
           05 P-ELEMENT PIC 9V99 VALUE 0.

       01  Z-VECTOR.
           05 Z-ELEMENT PIC 9V99 VALUE 0.

       01  ALPHA-VALUE           PIC 9V999999 VALUE 0.
       01  BETA-VALUE            PIC 9V999999 VALUE 0.
       01  R-TRANSPOSE-R         PIC 9V999999 VALUE 0.
       01  R-TRANSPOSE-R-OLD     PIC 9V999999 VALUE 0.
       01  P-TRANSPOSE-A-P       PIC 9V999999 VALUE 0.
       01  R-NORM                PIC 9V999999 VALUE 0.

       01  I-J-K                 PIC 9(3) VALUE 0.
       01  TEMP-VALUE            PIC 9V999999 VALUE 0.
       01  SUM-VALUE             PIC 9V999999 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-MATRIX
           PERFORM INITIALIZE-VECTORS
           PERFORM CONJUGATE-GRADIENT-ALGORITHM
           PERFORM DISPLAY-RESULTS
           STOP RUN.

       INITIALIZE-MATRIX.
           MOVE 10 TO A-ROW(1) A-ROW(2) A-ROW(3) A-ROW(4) A-ROW(5)
           MOVE 2 TO A-ROW(1)(1) A-ROW(2)(1) A-ROW(3)(1) A-ROW(4)(1) A-ROW(5)(1)
           MOVE 3 TO A-ROW(1)(2) A-ROW(2)(2) A-ROW(3)(2) A-ROW(4)(2) A-ROW(5)(2)
           MOVE 4 TO A-ROW(1)(3) A-ROW(2)(3) A-ROW(3)(3) A-ROW(4)(3) A-ROW(5)(3)
           MOVE 5 TO A-ROW(1)(4) A-ROW(2)(4) A-ROW(3)(4) A-ROW(4)(4) A-ROW(5)(4)
           MOVE 6 TO A-ROW(1)(5) A-ROW(2)(5) A-ROW(3)(5) A-ROW(4)(5) A-ROW(5)(5)

           MOVE 1 TO B-VECTOR(1)
           MOVE 2 TO B-VECTOR(2)
           MOVE 3 TO B-VECTOR(3)
           MOVE 4 TO B-VECTOR(4)
           MOVE 5 TO B-VECTOR(5)
           GOBACK.

       INITIALIZE-VECTORS.
           PERFORM INITIALIZE-VECTOR-X
           PERFORM INITIALIZE-VECTOR-R
           PERFORM INITIALIZE-VECTOR-P
           GOBACK.

       INITIALIZE-VECTOR-X.
           MOVE 0 TO X-ELEMENT(1) X-ELEMENT(2) X-ELEMENT(3) 
                   X-ELEMENT(4) X-ELEMENT(5)
           GOBACK.

       INITIALIZE-VECTOR-R.
           PERFORM CALCULATE-R-VECTOR
           GOBACK.

       INITIALIZE-VECTOR-P.
           MOVE 1 TO P-ELEMENT(1) P-ELEMENT(2) P-ELEMENT(3) 
                   P-ELEMENT(4) P-ELEMENT(5)
           GOBACK.

       CALCULATE-R-VECTOR.
           PERFORM MULTIPLY-AX-AND-SUBTRACT-B
           GOBACK.

       MULTIPLY-AX-AND-SUBTRACT-B.
           PERFORM V-VECTOR-INITIALIZE
           PERFORM MULTIPLY-A-X
           PERFORM SUBTRACT-B-VECTOR
           GOBACK.

       V-VECTOR-INITIALIZE.
           MOVE 0 TO R-ELEMENT(1) R-ELEMENT(2) R-ELEMENT(3) 
                   R-ELEMENT(4) R-ELEMENT(5)
           GOBACK.

       MULTIPLY-A-X.
           PERFORM MULTIPLY-A-X-LOOP
           GOBACK.

       MULTIPLY-A-X-LOOP.
           MOVE 1 TO I-J-K
           MULTIPLY-LOOP.
               MOVE 0 TO SUM-VALUE
               MOVE 1 TO J-K
               MULTIPLY-INNER-LOOP.
                   COMPUTE SUM-VALUE = SUM-VALUE + 
                       A-ROW(I-J-K)(J-K) * X-ELEMENT(J-K)
                   ADD 1 TO J-K
                   IF J-K <= MATRIX-LENGTH GO TO MULTIPLY-INNER-LOOP
               END-PERFORM
               MOVE SUM-VALUE TO R-ELEMENT(I-J-K)
               ADD 1 TO I-J-K
               IF I-J-K <= MATRIX-LENGTH GO TO MULTIPLY-LOOP
           END-PERFORM.
           GOBACK.

       SUBTRACT-B-VECTOR.
           MOVE 1 TO I-J-K
           SUBTRACT-B-LOOP.
               COMPUTE R-ELEMENT(I-J-K) = R-ELEMENT(I-J-K) - B-VECTOR(I-J-K)
               ADD 1 TO I-J-K
               IF I-J-K <= MATRIX-LENGTH GO TO SUBTRACT-B-LOOP
           END-PERFORM.
           GOBACK.

       CONJUGATE-GRADIENT-ALGORITHM.
           PERFORM CALCULATE-R-NORM
           PERFORM INITIALIZE-ITERATION-COUNT

       CG-ITERATION.
           IF ITERATION-COUNT > MAX-ITERATIONS OR CONVERGED = "Y"
               GO TO CG-END
           END-IF

           PERFORM COMPUTE-ALPHA
           PERFORM UPDATE-X
           PERFORM UPDATE-R
           PERFORM COMPUTE-BETA
           PERFORM UPDATE-P
           PERFORM CALCULATE-R-NORM
           PERFORM CHECK-CONVERGENCE
           ADD 1 TO ITERATION-COUNT
           GO TO CG-ITERATION

       CG-END.
           GOBACK.

       COMPUTE-ALPHA.
           MOVE 0 TO SUM-VALUE
           PERFORM COMPUTE-R-TRANSPOSE-R
           MOVE R-TRANSPOSE-R TO ALPHA-VALUE
           PERFORM COMPUTE-P-TRANSPOSE-A-P
           COMPUTE ALPHA-VALUE = R-TRANSPOSE-R / P-TRANSPOSE-A-P
           GOBACK.

       COMPUTE-R-TRANSPOSE-R.
           MOVE 0 TO R-TRANSPOSE-R
           MOVE 1 TO I-J-K
           COMPUTE-R-TRANSPOSE-R-LOOP.
               COMPUTE R-TRANSPOSE-R = R-TRANSPOSE-R + 
                   R-ELEMENT(I-J-K) * R-ELEMENT(I-J-K)
               ADD 1 TO I-J-K
               IF I-J-K <= MATRIX-LENGTH GO TO COMPUTE-R-TRANSPOSE-R-LOOP
           END-PERFORM.
           GOBACK.

       COMPUTE-P-TRANSPOSE-A-P.
           MOVE 0 TO P-TRANSPOSE-A-P
           PERFORM MULTIPLY-A-P
           MOVE 1 TO I-J-K
           COMPUTE-P-TRANSPOSE-A-P-LOOP.
               COMPUTE P-TRANSPOSE-A-P = P-TRANSPOSE-A-P + 
                   P-ELEMENT(I-J-K) * Z-ELEMENT(I-J-K)
               ADD 1 TO I-J-K
               IF I-J-K <= MATRIX-LENGTH GO TO COMPUTE-P-TRANSPOSE-A-P-LOOP
           END-PERFORM.
           GOBACK.

       MULTIPLY-A-P.
           PERFORM V-VECTOR-INITIALIZE
           MOVE 1 TO I-J-K
           MULTIPLY-A-P-LOOP.
               MOVE 0 TO SUM-VALUE
               MOVE 1 TO J-K
               MULTIPLY-A-P-INNER-LOOP.
                   COMPUTE SUM-VALUE = SUM-VALUE + 
                       A-ROW(I-J-K)(J-K) * P-ELEMENT(J-K)
                   ADD 1 TO J-K
                   IF J-K <= MATRIX-LENGTH GO TO MULTIPLY-A-P-INNER-LOOP
               END-PERFORM
               MOVE SUM-VALUE TO Z-ELEMENT(I-J-K)
               ADD 1 TO I-J-K
               IF I-J-K <= MATRIX-LENGTH GO TO MULTIPLY-A-P-LOOP
           END-PERFORM.
           GOBACK.

       UPDATE-X.
           MOVE 1 TO I-J-K
           UPDATE-X-LOOP.
               COMPUTE X-ELEMENT(I-J-K) = X-ELEMENT(I-J-K) + 
                   ALPHA-VALUE * P-ELEMENT(I-J-K)
               ADD 1 TO I-J-K
               IF I-J-K <= MATRIX-LENGTH GO TO UPDATE-X-LOOP
           END-PERFORM.
           GOBACK.

       UPDATE-R.
           MOVE 1 TO I-J-K
           UPDATE-R-LOOP.
               COMPUTE R-ELEMENT(I-J-K) = R-ELEMENT(I-J-K) - 
                   ALPHA-VALUE * Z-ELEMENT(I-J-K)
               ADD 1 TO I-J-K
               IF I-J-K <= MATRIX-LENGTH GO TO UPDATE-R-LOOP
           END-PERFORM.
           GOBACK.

       COMPUTE-BETA.
           MOVE 0 TO R-TRANSPOSE-R-OLD
           MOVE 1 TO I-J-K
           COMPUTE-BETA-LOOP.
               COMPUTE R-TRANSPOSE-R-OLD = R-TRANSPOSE-R-OLD + 
                   R-ELEMENT(I-J-K) * R-ELEMENT(I-J-K)
               ADD 1 TO I-J-K
               IF I-J-K <= MATRIX-LENGTH GO TO COMPUTE-BETA-LOOP
           END-PERFORM
           COMPUTE BETA-VALUE = R-TRANSPOSE-R-OLD / R-TRANSPOSE-R
           GOBACK.

       UPDATE-P.
           MOVE 1 TO I-J-K
           UPDATE-P-LOOP.
               COMPUTE P-ELEMENT(I-J-K) = R-ELEMENT(I-J-K) + 
                   BETA-VALUE * P-ELEMENT(I-J-K)
               ADD 1 TO I-J-K
               IF I-J-K <= MATRIX-LENGTH GO TO UPDATE-P-LOOP
           END-PERFORM.
           GOBACK.

       CALCULATE-R-NORM.
           MOVE 0 TO R-NORM
           MOVE 1 TO I-J-K
           CALCULATE-NORM-LOOP.
               COMPUTE R-NORM = R-NORM + 
                   R-ELEMENT(I-J-K) * R-ELEMENT(I-J-K)
               ADD 1 TO I-J-K
               IF I-J-K <= MATRIX-LENGTH GO TO CALCULATE-NORM-LOOP
           END-PERFORM
           COMPUTE R-NORM = FUNCTION SQRT(R-NORM)
           GOBACK.

       CHECK-CONVERGENCE.
           IF R-NORM < TOLERANCE
               MOVE "Y" TO CONVERGED
           END-IF
           GOBACK.

       INITIALIZE-ITERATION-COUNT.
           MOVE 0 TO ITERATION-COUNT
           GOBACK.

       DISPLAY-RESULTS.
           OPEN OUTPUT OUTPUT-FILE
           MOVE "Conjugate Gradient Method Results" TO OUTPUT-FIELD-1
           WRITE OUTPUT-RECORD
           MOVE "Iterations: " TO OUTPUT-FIELD-1
           MOVE ITERATION-COUNT TO OUTPUT-FIELD-2
           WRITE OUTPUT-RECORD
           MOVE "Final Residual Norm: " TO OUTPUT-FIELD-1
           MOVE R-NORM TO OUTPUT-FIELD-2
           WRITE OUTPUT-RECORD
           MOVE "Solution Vector:" TO OUTPUT-FIELD-1
           WRITE OUTPUT-RECORD
           MOVE 1 TO I-J-K
           DISPLAY-SOLUTION-LOOP.
               MOVE "X(" TO OUTPUT-FIELD-1
               MOVE I-J-K TO OUTPUT-FIELD-2
               MOVE ")" TO OUTPUT-FIELD-3
               MOVE X-ELEMENT(I-J-K) TO OUTPUT-FIELD-2
               WRITE OUTPUT-RECORD
               ADD 1 TO I-J-K
               IF I-J-K <= MATRIX-LENGTH GO TO DISPLAY-SOLUTION-LOOP
           END-PERFORM
           CLOSE OUTPUT-FILE
           GOBACK.
```

## Key Features of this Implementation:

1. **Matrix Representation**: Uses 2D arrays to store the coefficient matrix A
2. **Vector Operations**: Implements vector multiplication and addition operations
3. **Core Algorithm**: Implements the standard conjugate gradient iteration steps:
   - Compute alpha (step size)
   - Update solution vector x
   - Update residual vector r
   - Compute beta (conjugate direction update)
   - Update search direction p
4. **Convergence Check**: Tests if the residual norm falls below tolerance
5. **Output Generation**: Writes results to a text file including iterations and solution

## Usage Notes:

- This example solves a 5x5 linear system
- The matrix A and vector b are initialized with sample values
- The algorithm terminates when either maximum iterations are reached or convergence tolerance is met
- Results are written to "CG-RESULTS.TXT"

This implementation demonstrates how the conjugate gradient method can be translated from mathematical notation to COBOL programming language, maintaining the essential algorithmic structure while adapting to COBOL's procedural programming paradigm.

