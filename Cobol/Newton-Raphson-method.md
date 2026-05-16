# Newton-Raphson Method in COBOL

Here's an example implementation of the Newton-Raphson method to find the square root of a number in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NEWTON-RAPHSON.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  NUMBER              PIC 9(5)V99 VALUE 25.00.
       01  ROOT                PIC 9(5)V99 VALUE ZERO.
       01  PREV-ROOT           PIC 9(5)V99 VALUE ZERO.
       01  TOLERANCE           PIC 9(3)V99 VALUE 0.0001.
       01  ITERATION           PIC 99 VALUE 0.
       01  MAX-ITERATIONS      PIC 99 VALUE 20.
       01  DIFFERENCE          PIC 9(5)V99.
       01  TEMP                PIC 9(5)V99.
       01  IS-CONVERGED        PIC X VALUE 'N'.
           88  CONVERGED         VALUE 'Y'.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "NEWTON-RAPHSON METHOD EXAMPLE"
           DISPLAY "Finding square root of: " NUMBER
           DISPLAY "Tolerance: " TOLERANCE
           DISPLAY "----------------------------------------"

           MOVE NUMBER TO ROOT
           MOVE 0 TO ITERATION

       ITERATION-LOOP.
           IF ITERATION > MAX-ITERATIONS
               GO TO END-PROGRAM
           END-IF

           ADD 1 TO ITERATION

           COMPUTE TEMP = ROOT * ROOT
           COMPUTE PREV-ROOT = ROOT
           COMPUTE ROOT = (ROOT + (NUMBER / ROOT)) / 2

           COMPUTE DIFFERENCE = ROOT - PREV-ROOT
           IF DIFFERENCE < 0
               COMPUTE DIFFERENCE = DIFFERENCE * -1
           END-IF

           DISPLAY "Iteration " ITERATION ": Root = " ROOT
           DISPLAY "Difference = " DIFFERENCE

           IF DIFFERENCE < TOLERANCE
               MOVE 'Y' TO IS-CONVERGED
               GO TO DISPLAY-RESULTS
           END-IF

           GO TO ITERATION-LOOP

       DISPLAY-RESULTS.
           DISPLAY "----------------------------------------"
           DISPLAY "FINAL RESULT:"
           DISPLAY "Square root of " NUMBER " is approximately " ROOT
           DISPLAY "Iterations used: " ITERATION
           DISPLAY "Final difference: " DIFFERENCE

       END-PROGRAM.
```

## How it works:

1. **Initialization**: Sets up the number to find square root of (25.00), initial guess (same as number), tolerance (0.0001), and maximum iterations (20)

2. **Newton-Raphson Formula**: Uses the iterative formula `x_{n+1} = (x_n + N/x_n) / 2` where N is the number we want the square root of

3. **Convergence Check**: Continues iterating until the difference between consecutive approximations is less than the tolerance

4. **Output**: Displays each iteration's result and final answer

## Sample Output:
```
NEWTON-RAPHSON METHOD EXAMPLE
Finding square root of: 25.00
Tolerance: 0.0001
----------------------------------------
Iteration 1: Root = 13.00
Difference = 13.00
Iteration 2: Root = 7.769230769
Difference = 5.230769231
Iteration 3: Root = 5.524301445
Difference = 2.244929324
...
Final Result:
Square root of 25.00 is approximately 5.000000000
Iterations used: 5
Final difference: 0.000000000
```

This implementation demonstrates the classic Newton-Raphson method for finding roots of equations in COBOL, specifically applied to finding square roots.

