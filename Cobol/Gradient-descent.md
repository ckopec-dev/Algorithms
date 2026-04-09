# Gradient Descent Algorithm in COBOL

Here's an example of implementing a simple gradient descent algorithm in COBOL to minimize a cost function:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRADIENT-DESCENT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "DATA.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
          05 X-VALUE     PIC 9(5)V99.
          05 Y-VALUE     PIC 9(5)V99.

       WORKING-STORAGE SECTION.
       01 LEARNING-RATE    PIC 9V99 VALUE 0.01.
       01 ITERATIONS       PIC 9(4) VALUE 1000.
       01 TOLERANCE        PIC 9V999999 VALUE 0.000001.
       01 CONVERGED        PIC X VALUE 'N'.
       01 CURRENT-ERROR    PIC 9(5)V999999.
       01 PREVIOUS-ERROR   PIC 9(5)V999999.
       01 GRADIENT         PIC 9(5)V999999.
       01 THETA-0          PIC 9(5)V999999 VALUE 0.0.
       01 THETA-1          PIC 9(5)V999999 VALUE 0.0.
       01 TEMP-THETA-0     PIC 9(5)V999999.
       01 TEMP-THETA-1     PIC 9(5)V999999.
       01 SUM-ERROR        PIC 9(7)V999999.
       01 SUM-ERROR-X      PIC 9(7)V999999.
       01 SUM-ERROR-X2     PIC 9(7)V999999.
       01 M-VALUE          PIC 9(5)V999999.
       01 M-VALUE-X        PIC 9(5)V999999.
       01 M-VALUE-X2       PIC 9(5)V999999.
       01 COUNT            PIC 9(4) VALUE 0.
       01 TOTAL-ROWS       PIC 9(4) VALUE 0.
       01 I                PIC 9(4).
       01 J                PIC 9(4).
       01 TEMP             PIC 9(5)V999999.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "GRADIENT DESCENT ALGORITHM"
           DISPLAY "========================="
           
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE AT END GO TO END-READ
           MOVE INPUT-RECORD TO INPUT-RECORD
           ADD 1 TO TOTAL-ROWS
           PERFORM READ-ALL-RECORDS
           CLOSE INPUT-FILE

           DISPLAY "Total data points: " TOTAL-ROWS
           DISPLAY "Initial parameters: theta0=" THETA-0 ", theta1=" THETA-1
           DISPLAY "Learning rate: " LEARNING-RATE
           DISPLAY "Iterations: " ITERATIONS

           PERFORM GRADIENT-DESCENT-LOOP VARYING I FROM 1 BY 1 
               UNTIL I > ITERATIONS OR CONVERGED = 'Y'

           DISPLAY "Final parameters:"
           DISPLAY "theta0 = " THETA-0
           DISPLAY "theta1 = " THETA-1
           DISPLAY "Final error = " CURRENT-ERROR

           STOP RUN.

       READ-ALL-RECORDS.
           READ INPUT-FILE AT END GO TO END-READ
           ADD 1 TO TOTAL-ROWS
           GO TO READ-ALL-RECORDS.
       END-READ.

       GRADIENT-DESCENT-LOOP.
           MOVE 0 TO SUM-ERROR
           MOVE 0 TO SUM-ERROR-X
           MOVE 0 TO SUM-ERROR-X2

           PERFORM CALCULATE-GRADIENTS

           COMPUTE TEMP-THETA-0 = THETA-0 - (LEARNING-RATE * SUM-ERROR / TOTAL-ROWS)
           COMPUTE TEMP-THETA-1 = THETA-1 - (LEARNING-RATE * SUM-ERROR-X / TOTAL-ROWS)

           COMPUTE CURRENT-ERROR = 0
           PERFORM CALCULATE-ERROR

           IF I > 1
               COMPUTE TEMP = ABS(PREVIOUS-ERROR - CURRENT-ERROR)
               IF TEMP < TOLERANCE
                   MOVE 'Y' TO CONVERGED
               END-IF
           END-IF

           MOVE CURRENT-ERROR TO PREVIOUS-ERROR
           MOVE TEMP-THETA-0 TO THETA-0
           MOVE TEMP-THETA-1 TO THETA-1

           IF I MOD 100 = 0 OR I = 1
               DISPLAY "Iteration " I ": Error = " CURRENT-ERROR
           END-IF.

       CALCULATE-GRADIENTS.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > TOTAL-ROWS
               COMPUTE GRADIENT = (THETA-0 + THETA-1 * X-VALUE) - Y-VALUE
               ADD GRADIENT TO SUM-ERROR
               COMPUTE GRADIENT = GRADIENT * X-VALUE
               ADD GRADIENT TO SUM-ERROR-X
           END-PERFORM.

       CALCULATE-ERROR.
           COMPUTE CURRENT-ERROR = 0
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > TOTAL-ROWS
               COMPUTE GRADIENT = (THETA-0 + THETA-1 * X-VALUE) - Y-VALUE
               COMPUTE GRADIENT = GRADIENT * GRADIENT
               ADD GRADIENT TO CURRENT-ERROR
           END-PERFORM
           COMPUTE CURRENT-ERROR = CURRENT-ERROR / (2 * TOTAL-ROWS).
```

## Explanation

This COBOL program implements a gradient descent algorithm for linear regression:

### Key Components:
1. **Data Input**: Reads data points from a file (X, Y values)
2. **Parameters**: 
   - `THETA-0` and `THETA-1`: Model parameters (intercept and slope)
   - `LEARNING-RATE`: Step size for updates
   - `ITERATIONS`: Maximum number of iterations
3. **Gradient Calculation**: Computes partial derivatives of the cost function
4. **Parameter Updates**: Updates theta values using gradient descent formula
5. **Convergence Check**: Stops when error change is below tolerance

### Algorithm Steps:
1. Initialize parameters to zero
2. For each iteration:
   - Calculate gradients using current parameters
   - Update parameters using gradient descent formula
   - Check for convergence
3. Output final parameters and error

### Mathematical Formula:
- Hypothesis: h(x) = θ₀ + θ₁x
- Cost function: J(θ₀, θ₁) = ½∑(h(x) - y)²
- Gradient updates: θ₀ := θ₀ - α∇J(θ₀, θ₁)
- Gradient updates: θ₁ := θ₁ - α∇J(θ₀, θ₁)

This implementation demonstrates the core gradient descent algorithm in COBOL, though in practice, more efficient languages would be used for numerical computing tasks.

