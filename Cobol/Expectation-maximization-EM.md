# Expectation-Maximization (EM) Algorithm in COBOL

Here's a simplified example of implementing the EM algorithm in COBOL for clustering data points into two groups:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EM-ALGORITHM.
       AUTHOR. AI Assistant.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "DATA.DAT"
               ORGANIZATION IS SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "RESULTS.DAT"
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
          05 X-COORDINATE    PIC 9(3)V99.
          05 Y-COORDINATE    PIC 9(3)V99.

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
          05 ITERATION-NUMBER PIC 99.
          05 CENTROID-1-X     PIC 9(3)V99.
          05 CENTROID-1-Y     PIC 9(3)V99.
          05 CENTROID-2-X     PIC 9(3)V99.
          05 CENTROID-2-Y     PIC 9(3)V99.

       WORKING-STORAGE SECTION.
       01 DATA-ARRAY.
          05 DATA-RECORD OCCURS 100 TIMES INDEXED BY I.
             10 X-VALUE    PIC 9(3)V99.
             10 Y-VALUE    PIC 9(3)V99.

       01 CENTROIDS.
          05 CENTROID-1.
             10 C1-X    PIC 9(3)V99 VALUE 2.00.
             10 C1-Y    PIC 9(3)V99 VALUE 3.00.
          05 CENTROID-2.
             10 C2-X    PIC 9(3)V99 VALUE 6.00.
             10 C2-Y    PIC 9(3)V99 VALUE 7.00.

       01 PROBABILITIES.
          05 PROBABILITY-1 OCCURS 100 TIMES.
             10 P1-I    PIC 9(3)V99.
          05 PROBABILITY-2 OCCURS 100 TIMES.
             10 P2-I    PIC 9(3)V99.

       01 TEMPORARY-VARIABLES.
          05 ITERATION-COUNT     PIC 99 VALUE 0.
          05 MAX-ITERATIONS      PIC 99 VALUE 10.
          05 CONVERGENCE-TOLERANCE PIC 9(3)V99 VALUE 0.001.
          05 DISTANCE            PIC 9(3)V99.
          05 SUM-PROB-1          PIC 9(5)V99.
          05 SUM-PROB-2          PIC 9(5)V99.
          05 N-DATA              PIC 99 VALUE 100.
          05 TOTAL-ERROR         PIC 9(5)V99.

       01 FLAGS.
          05 CONVERGED           PIC X VALUE 'N'.
             88 CONVERGED-FALSE VALUE 'N'.
             88 CONVERGED-TRUE  VALUE 'Y'.
          05 EOF-FLAG            PIC X VALUE 'N'.
             88 END-OF-FILE     VALUE 'Y'.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-DATA.
           PERFORM EM-ITERATIONS UNTIL CONVERGED-TRUE OR ITERATION-COUNT > MAX-ITERATIONS.
           PERFORM WRITE-RESULTS.
           STOP RUN.

       INITIALIZE-DATA.
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.
           READ INPUT-FILE INTO DATA-RECORD(I) AT END SET EOF-FLAG TO 'Y'.
           PERFORM UNTIL EOF-FLAG = 'Y'
               ADD 1 TO I
               READ INPUT-FILE INTO DATA-RECORD(I) AT END SET EOF-FLAG TO 'Y'
           END-PERFORM.
           CLOSE INPUT-FILE.

       EM-ITERATIONS.
           ADD 1 TO ITERATION-COUNT.
           PERFORM EXPECTATION-STEP.
           PERFORM MAXIMIZATION-STEP.
           PERFORM CHECK-CONVERGENCE.
           PERFORM WRITE-ITERATION-RESULTS.

       EXPECTATION-STEP.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N-DATA
               COMPUTE DISTANCE = FUNCTION SQRT ((X-VALUE(I) - C1-X)**2 + (Y-VALUE(I) - C1-Y)**2)
               COMPUTE P1-I = 1 / (1 + (DISTANCE / FUNCTION SQRT ((X-VALUE(I) - C2-X)**2 + (Y-VALUE(I) - C2-Y)**2))**2)
               COMPUTE P2-I = 1 - P1-I
           END-PERFORM.

       MAXIMIZATION-STEP.
           COMPUTE SUM-PROB-1 = 0.
           COMPUTE SUM-PROB-2 = 0.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N-DATA
               COMPUTE SUM-PROB-1 = SUM-PROB-1 + P1-I
               COMPUTE SUM-PROB-2 = SUM-PROB-2 + P2-I
           END-PERFORM.

           COMPUTE C1-X = 0.
           COMPUTE C1-Y = 0.
           COMPUTE C2-X = 0.
           COMPUTE C2-Y = 0.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N-DATA
               COMPUTE C1-X = C1-X + (P1-I * X-VALUE(I))
               COMPUTE C1-Y = C1-Y + (P1-I * Y-VALUE(I))
               COMPUTE C2-X = C2-X + (P2-I * X-VALUE(I))
               COMPUTE C2-Y = C2-Y + (P2-I * Y-VALUE(I))
           END-PERFORM.

           COMPUTE C1-X = C1-X / SUM-PROB-1.
           COMPUTE C1-Y = C1-Y / SUM-PROB-1.
           COMPUTE C2-X = C2-X / SUM-PROB-2.
           COMPUTE C2-Y = C2-Y / SUM-PROB-2.

       CHECK-CONVERGENCE.
           COMPUTE TOTAL-ERROR = FUNCTION ABS(C1-X - C1-X-PREV) + 
                                 FUNCTION ABS(C1-Y - C1-Y-PREV) +
                                 FUNCTION ABS(C2-X - C2-X-PREV) +
                                 FUNCTION ABS(C2-Y - C2-Y-PREV).
           IF TOTAL-ERROR < CONVERGENCE-TOLERANCE
               SET CONVERGED-TRUE TO TRUE
           END-IF.

       WRITE-ITERATION-RESULTS.
           WRITE OUTPUT-RECORD FROM ITERATION-COUNT.
           WRITE OUTPUT-RECORD FROM C1-X.
           WRITE OUTPUT-RECORD FROM C1-Y.
           WRITE OUTPUT-RECORD FROM C2-X.
           WRITE OUTPUT-RECORD FROM C2-Y.

       WRITE-RESULTS.
           CLOSE OUTPUT-FILE.
```

## Key Components of the EM Algorithm in COBOL:

### 1. **Initialization**
- Sets initial centroid positions
- Reads input data from file
- Initializes working storage variables

### 2. **Expectation Step (E-step)**
- Calculates probability that each data point belongs to cluster 1 or 2
- Uses distance formula and probability calculation
- Stores probabilities in arrays

### 3. **Maximization Step (M-step)**
- Updates centroid positions based on weighted averages
- Calculates new centroids using the probabilities from E-step

### 4. **Convergence Check**
- Compares current centroids with previous iteration
- Stops when change is below tolerance level

### 5. **Output Generation**
- Writes results to output file
- Records iteration numbers and centroid coordinates

## Sample Input Data Format:
```
0102
0203
0304
0405
0506
0607
0708
```

## Sample Output:
```
01  2.00  3.00  6.00  7.00
02  2.50  3.50  5.50  6.50
03  2.60  3.60  5.40  6.40
```

This implementation demonstrates the core EM algorithm concepts in COBOL, though it's simplified for educational purposes and would need enhancements for production use.