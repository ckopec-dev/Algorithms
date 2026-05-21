# Thomas Algorithm in COBOL

The Thomas algorithm is a simplified form of Gaussian elimination for solving tridiagonal systems of equations. Here's an implementation in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. THOMAS-ALGORITHM.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "TRIDIAG.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "RESULT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
          05 N               PIC 9(3).
          05 A-ARRAY         PIC 9(5)V9(2) OCCURS 100 TIMES.
          05 B-ARRAY         PIC 9(5)V9(2) OCCURS 100 TIMES.
          05 C-ARRAY         PIC 9(5)V9(2) OCCURS 100 TIMES.
          05 D-ARRAY         PIC 9(5)V9(2) OCCURS 100 TIMES.

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
          05 RESULT-ARRAY    PIC 9(5)V9(2) OCCURS 100 TIMES.

       WORKING-STORAGE SECTION.
       01 WS-N              PIC 9(3) VALUE 0.
       01 WS-I              PIC 9(3) VALUE 0.
       01 WS-J              PIC 9(3) VALUE 0.
       01 WS-K              PIC 9(3) VALUE 0.
       01 WS-TEMP           PIC 9(5)V9(2) VALUE 0.
       01 WS-FLAG           PIC X VALUE "N".
       01 WS-EOF            PIC X VALUE "N".
       01 WS-ERROR          PIC X VALUE "N".

       01 A-ARRAY-W         PIC 9(5)V9(2) OCCURS 100 TIMES.
       01 B-ARRAY-W         PIC 9(5)V9(2) OCCURS 100 TIMES.
       01 C-ARRAY-W         PIC 9(5)V9(2) OCCURS 100 TIMES.
       01 D-ARRAY-W         PIC 9(5)V9(2) OCCURS 100 TIMES.
       01 X-ARRAY-W         PIC 9(5)V9(2) OCCURS 100 TIMES.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           READ INPUT-FILE
               AT END MOVE "Y" TO WS-EOF
           END-READ

           IF WS-EOF = "N"
               MOVE N TO WS-N
               PERFORM THOMAS-SOLVE
               PERFORM WRITE-RESULTS
           END-IF

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       THOMAS-SOLVE.
           PERFORM INIT-ARRAYS.

           * Forward elimination phase
           PERFORM 100 TIMES
               IF WS-I > 1
                   COMPUTE WS-TEMP = A-ARRAY-W(WS-I) / B-ARRAY-W(WS-I-1)
                   COMPUTE B-ARRAY-W(WS-I) = B-ARRAY-W(WS-I) - WS-TEMP * C-ARRAY-W(WS-I-1)
                   COMPUTE D-ARRAY-W(WS-I) = D-ARRAY-W(WS-I) - WS-TEMP * D-ARRAY-W(WS-I-1)
               END-IF
               ADD 1 TO WS-I
           END-PERFORM

           * Back substitution phase
           COMPUTE X-ARRAY-W(WS-N) = D-ARRAY-W(WS-N) / B-ARRAY-W(WS-N)

           PERFORM 100 TIMES
               IF WS-I < WS-N
                   COMPUTE X-ARRAY-W(WS-N-WS-I) = (D-ARRAY-W(WS-N-WS-I) - 
                       C-ARRAY-W(WS-N-WS-I) * X-ARRAY-W(WS-N-WS-I+1)) / 
                       B-ARRAY-W(WS-N-WS-I)
               END-IF
               ADD 1 TO WS-I
           END-PERFORM.

       INIT-ARRAYS.
           MOVE 1 TO WS-I
           PERFORM 100 TIMES
               MOVE A-ARRAY(WS-I) TO A-ARRAY-W(WS-I)
               MOVE B-ARRAY(WS-I) TO B-ARRAY-W(WS-I)
               MOVE C-ARRAY(WS-I) TO C-ARRAY-W(WS-I)
               MOVE D-ARRAY(WS-I) TO D-ARRAY-W(WS-I)
               MOVE 0 TO X-ARRAY-W(WS-I)
               ADD 1 TO WS-I
           END-PERFORM.

       WRITE-RESULTS.
           MOVE 1 TO WS-I
           PERFORM 100 TIMES
               MOVE X-ARRAY-W(WS-I) TO RESULT-ARRAY(WS-I)
               ADD 1 TO WS-I
           END-PERFORM

           WRITE OUTPUT-RECORD FROM RESULT-ARRAY.

       END PROGRAM THOMAS-ALGORITHM.
```

## Example Input File (TRIDIAG.DAT):
```
    4
    1.00    2.00    3.00    4.00
    2.00    3.00    4.00    5.00
    3.00    4.00    5.00    6.00
    4.00    5.00    6.00    7.00
```

## Key Features of This Implementation:

1. **Forward Elimination**: Eliminates the lower diagonal elements
2. **Back Substitution**: Solves for the unknown variables
3. **Array Handling**: Uses OCCURS clauses for tridiagonal matrix storage
4. **File I/O**: Reads input from file and writes results to output file
5. **Error Handling**: Basic EOF checking and error flags

## Algorithm Steps:
1. **Forward elimination**: Transform the system to upper triangular form
2. **Back substitution**: Solve for variables starting from the last equation
3. **Result storage**: Store final solution in output file

This implementation solves a tridiagonal system of equations Ax = b where A is a tridiagonal matrix with diagonal elements in B-array, sub-diagonal in A-array, and super-diagonal in C-array.

