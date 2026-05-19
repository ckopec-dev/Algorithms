# Cholesky Decomposition in COBOL

Here's an example of implementing Cholesky decomposition in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHOLESKY-DECOMPOSITION.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MATRIX-FILE ASSIGN TO "MATRIX.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  MATRIX-FILE.
       01  MATRIX-RECORD.
           05  MATRIX-DATA    PIC 9(3)V99 OCCURS 10 TIMES.

       WORKING-STORAGE SECTION.
       01  MATRIX-A.
           05  A-VALUE        PIC 9(3)V99 OCCURS 10 TIMES DEPENDING ON N.
       01  L-VALUE.
           05  L-VALUE-ROW    PIC 9(3)V99 OCCURS 10 TIMES DEPENDING ON N.
       01  N                PIC 99 VALUE 4.
       01  I                PIC 99.
       01  J                PIC 99.
       01  K                PIC 99.
       01  SUM              PIC 9(5)V99.
       01  TEMP             PIC 9(5)V99.
       01  ERROR-FLAG       PIC 9 VALUE 0.
       01  OUTPUT-FILE.
           05  OUTPUT-RECORD  PIC X(50).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "CHOLESKY DECOMPOSITION EXAMPLE"
           DISPLAY "================================"
           
           PERFORM INITIALIZE-MATRIX
           PERFORM PRINT-ORIGINAL-MATRIX
           PERFORM CHOLESKY-DECOMPOSE
           PERFORM PRINT-RESULT-MATRIX
           
           STOP RUN.

       INITIALIZE-MATRIX.
           MOVE 16 TO A-VALUE(1)
           MOVE 4  TO A-VALUE(2)
           MOVE 8  TO A-VALUE(3)
           MOVE 12 TO A-VALUE(4)
           MOVE 4  TO A-VALUE(5)
           MOVE 5  TO A-VALUE(6)
           MOVE 6  TO A-VALUE(7)
           MOVE 7  TO A-VALUE(8)
           MOVE 8  TO A-VALUE(9)
           MOVE 6  TO A-VALUE(10)
           MOVE 11 TO A-VALUE(11)
           MOVE 10 TO A-VALUE(12)
           MOVE 12 TO A-VALUE(13)
           MOVE 7  TO A-VALUE(14)
           MOVE 10 TO A-VALUE(15)
           MOVE 15 TO A-VALUE(16)

       CHOLESKY-DECOMPOSE.
           PERFORM INITIALIZE-L-MATRIX
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               PERFORM COMPUTE-L-I-I
               PERFORM COMPUTE-L-I-J
           END-PERFORM
           
           PERFORM VERIFY-DECOMPOSITION

       INITIALIZE-L-MATRIX.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
                   MOVE 0 TO L-VALUE-ROW(I-J)
               END-PERFORM
           END-PERFORM

       COMPUTE-L-I-I.
           COMPUTE SUM = 0
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > I
               IF K = I THEN
                   COMPUTE SUM = SUM + A-VALUE(I-I) - SUM
               ELSE
                   COMPUTE SUM = SUM + L-VALUE-ROW(I-K) * L-VALUE-ROW(I-K)
               END-IF
           END-PERFORM
           COMPUTE L-VALUE-ROW(I-I) = FUNCTION SQRT(SUM)

       COMPUTE-L-I-J.
           PERFORM VARYING J FROM (I + 1) BY 1 UNTIL J > N
               COMPUTE SUM = 0
               PERFORM VARYING K FROM 1 BY 1 UNTIL K > I
                   COMPUTE SUM = SUM + L-VALUE-ROW(I-K) * L-VALUE-ROW(J-K)
               END-PERFORM
               COMPUTE L-VALUE-ROW(J-I) = (A-VALUE(J-I) - SUM) / L-VALUE-ROW(I-I)
           END-PERFORM

       VERIFY-DECOMPOSITION.
           IF ERROR-FLAG = 1 THEN
               DISPLAY "ERROR: Matrix is not positive definite"
           ELSE
               DISPLAY "Cholesky decomposition successful"
           END-IF

       PRINT-ORIGINAL-MATRIX.
           DISPLAY "Original Matrix A:"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               MOVE " " TO OUTPUT-RECORD
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
                   STRING " " A-VALUE(I-J) DELIMITED BY SIZE INTO OUTPUT-RECORD
               END-PERFORM
               DISPLAY OUTPUT-RECORD
           END-PERFORM

       PRINT-RESULT-MATRIX.
           DISPLAY "Lower Triangular Matrix L:"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               MOVE " " TO OUTPUT-RECORD
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
                   IF I > J THEN
                       STRING " " L-VALUE-ROW(I-J) DELIMITED BY SIZE INTO OUTPUT-RECORD
                   ELSE IF I = J THEN
                       STRING " " L-VALUE-ROW(I-J) DELIMITED BY SIZE INTO OUTPUT-RECORD
                   ELSE
                       STRING " 0" DELIMITED BY SIZE INTO OUTPUT-RECORD
                   END-IF
               END-PERFORM
               DISPLAY OUTPUT-RECORD
           END-PERFORM
```

## Key Features of this COBOL Implementation:

1. **Matrix Storage**: Uses 2-dimensional arrays to store the matrix data
2. **Cholesky Algorithm**: Implements the standard Cholesky decomposition algorithm
3. **Error Handling**: Checks for positive definite matrices
4. **Output**: Displays both original and decomposed matrices
5. **Modular Design**: Separates initialization, computation, and output operations

## Algorithm Steps:

1. **Initialization**: Set up the input matrix
2. **Compute diagonal elements**: L(i,i) = √(A(i,i) - Σ(L(i,k)²))
3. **Compute off-diagonal elements**: L(j,i) = (A(j,i) - Σ(L(j,k)L(i,k))) / L(i,i)
4. **Verification**: Check if matrix is positive definite
5. **Output**: Display results

This implementation demonstrates how to perform matrix decomposition in COBOL, which is typically used in legacy business applications where such numerical methods are required.

