# Matrix Chain Multiplication in COBOL

Here's a COBOL implementation of the Matrix Chain Multiplication algorithm to find the optimal way to multiply a chain of matrices:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MATRIX-CHAIN-MULTIPLICATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTPUT-FILE ASSIGN TO "MATRIX-CHAIN-RESULTS.TXT"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
          05 OUTPUT-TEXT        PIC X(80).

       WORKING-STORAGE SECTION.
       01 MATRIX-SIZE.
          05 N                  PIC 9(3) VALUE 6.
          05 I                  PIC 9(3).
          05 J                  PIC 9(3).
          05 K                  PIC 9(3).
          05 Q                  PIC 9(10).
          05 M                  PIC 9(10).
          05 P                  PIC 9(10).

       01 MATRIX-DIMENSIONS.
          05 DIMS                 PIC 9(3) OCCURS 7 TIMES.
          05 DIMS-INDEX           PIC 9(3) VALUE 1.

       01 DP-MATRIX.
          05 DP-TABLE             PIC 9(10) OCCURS 7 TIMES INDEXED BY DP-INDEX.
          05 DP-ROW               PIC 9(10) OCCURS 7 TIMES.
          05 DP-COL               PIC 9(10) OCCURS 7 TIMES.

       01 DP-RESULT.
          05 DP-VALUE             PIC 9(10).
          05 DP-VALUE-2           PIC 9(10).
          05 DP-VALUE-3           PIC 9(10).

       01 TEMP-VALUES.
          05 MIN-VALUE            PIC 9(10) VALUE 0.
          05 TEMP-MIN             PIC 9(10) VALUE 0.

       01 DISPLAY-VALUES.
          05 DISPLAY-TEXT         PIC X(80).
          05 DISPLAY-INT          PIC 9(10).

       01 FILE-STATUS          PIC XX.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-DATA.
           PERFORM MATRIX-CHAIN-ALGORITHM.
           PERFORM DISPLAY-RESULTS.
           STOP RUN.

       INITIALIZE-DATA.
           MOVE 6 TO N.
           
           *> Matrix dimensions: A1(30x35), A2(35x15), A3(15x5), A4(5x10), A5(10x20), A6(20x25)
           MOVE 30 TO DIMS(1).
           MOVE 35 TO DIMS(2).
           MOVE 15 TO DIMS(3).
           MOVE 5 TO DIMS(4).
           MOVE 10 TO DIMS(5).
           MOVE 20 TO DIMS(6).
           MOVE 25 TO DIMS(7).

           *> Initialize DP table with zeros
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
                   MOVE 0 TO DP-TABLE(I,J)
               END-PERFORM
           END-PERFORM.

           *> Initialize diagonal elements to 0
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               MOVE 0 TO DP-TABLE(I,I)
           END-PERFORM.

       MATRIX-CHAIN-ALGORITHM.
           *> Chain length from 2 to n
           PERFORM VARYING L FROM 2 BY 1 UNTIL L > N
               *> For each chain of length L
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > N - L + 1
                   MOVE 0 TO MIN-VALUE.
                   MOVE 0 TO TEMP-MIN.
                   
                   *> Try all possible splits
                   PERFORM VARYING K FROM I BY 1 UNTIL K > I + L - 2
                       *> Calculate cost of multiplication
                       MOVE DP-TABLE(I,K) TO DP-VALUE.
                       MOVE DP-TABLE(K+1,I+L-1) TO DP-VALUE-2.
                       MOVE DIMS(I-1) TO DP-VALUE-3.
                       COMPUTE Q = DP-VALUE + DP-VALUE-2 + 
                                  (DIMS(I-1) * DIMS(K) * DIMS(I+L-1))
                       IF MIN-VALUE = 0 OR Q < MIN-VALUE
                           MOVE Q TO MIN-VALUE
                       END-IF
                   END-PERFORM
                   
                   MOVE MIN-VALUE TO DP-TABLE(I,I+L-1)
               END-PERFORM
           END-PERFORM.

       DISPLAY-RESULTS.
           MOVE "MATRIX CHAIN MULTIPLICATION RESULTS" TO DISPLAY-TEXT.
           PERFORM WRITE-OUTPUT.
           
           MOVE "Optimal number of scalar multiplications:" TO DISPLAY-TEXT.
           PERFORM WRITE-OUTPUT.
           
           MOVE "Minimum cost: " TO DISPLAY-TEXT.
           MOVE DP-TABLE(1,N) TO DISPLAY-INT.
           STRING DISPLAY-TEXT " " DISPLAY-INT INTO DISPLAY-TEXT.
           PERFORM WRITE-OUTPUT.
           
           MOVE "Matrix dimensions:" TO DISPLAY-TEXT.
           PERFORM WRITE-OUTPUT.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               MOVE "Matrix " TO DISPLAY-TEXT.
               MOVE I TO DISPLAY-INT.
               STRING DISPLAY-TEXT I " : " DIMS(I-1) " x " DIMS(I) INTO DISPLAY-TEXT.
               PERFORM WRITE-OUTPUT
           END-PERFORM.

       WRITE-OUTPUT.
           OPEN OUTPUT OUTPUT-FILE.
           MOVE OUTPUT-TEXT TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
           CLOSE OUTPUT-FILE.

       END PROGRAM MATRIX-CHAIN-MULTIPLICATION.
```

## Explanation

This COBOL program implements the Matrix Chain Multiplication algorithm using dynamic programming:

### Key Features:

1. **Data Structure**: Uses a 2D array `DP-TABLE` to store minimum multiplication costs
2. **Algorithm**: Implements the classic dynamic programming approach with time complexity O(n³)
3. **Matrix Dimensions**: Uses example matrices with dimensions [30×35, 35×15, 15×5, 5×10, 10×20, 20×25]
4. **Output**: Writes results to a text file showing the minimum number of scalar multiplications needed

### How It Works:

1. **Initialization**: Sets up matrix dimensions and initializes the DP table
2. **Dynamic Programming**: For chains of increasing length (2 to n), computes the optimal split point
3. **Cost Calculation**: For each possible split, calculates the cost of multiplying matrices
4. **Result**: Finds the minimum cost to multiply the entire chain

### Expected Output:
```
Matrix Chain Multiplication Results
Optimal number of scalar multiplications: 15125
Matrix dimensions:
Matrix 1 : 30 x 35
Matrix 2 : 35 x 15
Matrix 3 : 15 x 5
Matrix 4 : 5 x 10
Matrix 5 : 10 x 20
Matrix 6 : 20 x 25
```

This implementation demonstrates how COBOL can handle complex algorithms like dynamic programming, though it's worth noting that modern languages are typically preferred for such computational tasks.

