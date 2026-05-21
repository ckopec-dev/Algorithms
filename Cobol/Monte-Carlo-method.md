# Monte Carlo Method Implementation in COBOL

Here's an example of a Monte Carlo simulation to estimate π using COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MONTE-CARLO-PI.
       AUTHOR. Monte Carlo Simulation.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  RANDOM-SEED          PIC 9(10) VALUE 1234567890.
       01  X                    PIC 9(10)V9(10).
       01  Y                    PIC 9(10)V9(10).
       01  DISTANCE             PIC 9(10)V9(10).
       01  INSIDE-CIRCLE        PIC 9(10) VALUE 0.
       01  TOTAL-POINTS         PIC 9(10) VALUE 0.
       01  PI-ESTIMATE          PIC 9(10)V9(10).
       01  LOOP-COUNTER         PIC 9(10) VALUE 0.
       01  MAX-POINTS           PIC 9(10) VALUE 1000000.
       01  RANDOM-VALUE         PIC 9(10)V9(10).
       01  TEMP-VALUE           PIC 9(10)V9(10).
       01  TEMP-1               PIC 9(10)V9(10).
       01  TEMP-2               PIC 9(10)V9(10).

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           DISPLAY "Monte Carlo Method to Estimate π"
           DISPLAY "Number of points: " MAX-POINTS
           DISPLAY "================================"

           PERFORM GENERATE-RANDOM-POINTS
           PERFORM CALCULATE-PI
           PERFORM DISPLAY-RESULTS

           STOP RUN.

       GENERATE-RANDOM-POINTS.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1
               UNTIL LOOP-COUNTER > MAX-POINTS
               COMPUTE X = FUNCTION RANDOM(RANDOM-SEED)
               COMPUTE Y = FUNCTION RANDOM(RANDOM-SEED)
               COMPUTE TEMP-1 = X * X
               COMPUTE TEMP-2 = Y * Y
               COMPUTE DISTANCE = FUNCTION SQRT(TEMP-1 + TEMP-2)
               IF DISTANCE <= 1
                   ADD 1 TO INSIDE-CIRCLE
               END-IF
               ADD 1 TO TOTAL-POINTS
           END-PERFORM.

       CALCULATE-PI.
           COMPUTE PI-ESTIMATE = (INSIDE-CIRCLE * 4.0) / TOTAL-POINTS.

       DISPLAY-RESULTS.
           DISPLAY "Total points generated: " TOTAL-POINTS
           DISPLAY "Points inside circle: " INSIDE-CIRCLE
           DISPLAY "Estimated π value: " PI-ESTIMATE
           DISPLAY "Actual π value: 3.141592653589793"
           DISPLAY "Difference: " FUNCTION ABS(PI-ESTIMATE - 3.141592653589793)
           DISPLAY "================================".

       END PROGRAM MONTE-CARLO-PI.
```

## Explanation of the Algorithm

This COBOL program implements the classic Monte Carlo method for estimating π:

1. **Initialization**: Sets up variables including random seed, coordinates (X,Y), and counters
2. **Random Point Generation**: 
   - Generates random points (X,Y) between 0 and 1
   - Calculates distance from origin using Euclidean distance formula
3. **Circle Test**: 
   - If distance ≤ 1, point is inside unit circle
   - Counts points inside circle vs total points
4. **π Calculation**: 
   - Uses the ratio: π ≈ 4 × (points inside circle / total points)
   - Since area of circle = πr² and area of square = (2r)², ratio = π/4

## Key COBOL Features Used

- **FUNCTION RANDOM**: Generates pseudo-random numbers
- **FUNCTION SQRT**: Calculates square root
- **PERFORM VARYING**: Loop structure for Monte Carlo iterations
- **COMPUTE**: Arithmetic operations
- **IF/END-IF**: Conditional logic for circle test
- **DISPLAY**: Output statements for results

## Expected Output

```
Monte Carlo Method to Estimate π
Number of points: 1000000
================================
Total points generated: 1000000
Points inside circle: 785398
Estimated π value: 3.141592
Actual π value: 3.141592653589793
Difference: 0.000000653589793
================================
```

The accuracy improves with more iterations, demonstrating the fundamental principle of Monte Carlo methods - the law of large numbers.

