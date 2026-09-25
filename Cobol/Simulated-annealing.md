# Simulated Annealing Algorithm in COBOL

Here's an example implementation of a simulated annealing algorithm in COBOL for solving a simple optimization problem:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMULATED-ANNEALING.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTPUT-FILE ASSIGN TO "SA-RESULTS.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD    PIC X(80).

       WORKING-STORAGE SECTION.
       01 TEMP-VALUES.
          05 CURRENT-TEMP     PIC 9(5)V99 VALUE 1000.00.
          05 FINAL-TEMP       PIC 9(5)V99 VALUE 0.01.
          05 COOLING-RATE     PIC 9V99 VALUE 0.95.
          05 ACCEPT-PROB      PIC 9V9999.
          05 RAND-VALUE       PIC 9V9999.
          05 ITERATION-COUNT  PIC 9(6) VALUE 0.

       01 SOLUTIONS.
          05 CURRENT-SOLUTION PIC 9(4) VALUE 0.
          05 BEST-SOLUTION    PIC 9(4) VALUE 0.
          05 NEW-SOLUTION     PIC 9(4) VALUE 0.
          05 CURRENT-FITNESS  PIC 9(6)V99 VALUE 0.00.
          05 BEST-FITNESS     PIC 9(6)V99 VALUE 9999.00.
          05 NEW-FITNESS      PIC 9(6)V99 VALUE 0.00.
          05 DELTA-FITNESS    PIC 9(6)V99 VALUE 0.00.

       01 RANDOM-SEED        PIC 9(9) VALUE 123456789.
       01 RANDOM-VALUE       PIC 9(9).
       01 RANDOM-RESULT      PIC 9V9999.

       01 MAIN-CONTROL.
          05 LOOP-COUNTER     PIC 9(6) VALUE 0.
          05 MAX-ITERATIONS   PIC 9(6) VALUE 1000.
          05 MAX-NEIGHBORS    PIC 9(3) VALUE 10.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-ALGORITHM
           PERFORM SIMULATED-ANNEALING-LOOP
           PERFORM WRITE-RESULTS
           STOP RUN.

       INITIALIZE-ALGORITHM.
           MOVE FUNCTION RANDOM(RANDOM-SEED) TO RANDOM-VALUE
           COMPUTE RANDOM-RESULT = RANDOM-VALUE / 1000000000.00
           COMPUTE CURRENT-SOLUTION = FUNCTION RANDOM(1000)
           COMPUTE NEW-SOLUTION = FUNCTION RANDOM(1000)
           PERFORM CALCULATE-FITNESS
           MOVE CURRENT-FITNESS TO BEST-FITNESS
           MOVE CURRENT-SOLUTION TO BEST-SOLUTION
           DISPLAY "INITIAL SOLUTION: " CURRENT-SOLUTION
           DISPLAY "INITIAL FITNESS: " CURRENT-FITNESS.

       SIMULATED-ANNEALING-LOOP.
           PERFORM UNTIL CURRENT-TEMP < FINAL-TEMP
               ADD 1 TO ITERATION-COUNT
               PERFORM GENERATE-NEIGHBOR
               PERFORM CALCULATE-FITNESS
               PERFORM ACCEPT-OR-REJECT
               PERFORM COOL-TEMPERATURE
               IF ITERATION-COUNT > MAX-ITERATIONS
                   DISPLAY "MAX ITERATIONS REACHED"
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM.

       GENERATE-NEIGHBOR.
           COMPUTE NEW-SOLUTION = CURRENT-SOLUTION + FUNCTION RANDOM(20) - 10
           IF NEW-SOLUTION < 0
               MOVE 0 TO NEW-SOLUTION
           END-IF
           IF NEW-SOLUTION > 9999
               MOVE 9999 TO NEW-SOLUTION
           END-IF.

       CALCULATE-FITNESS.
           COMPUTE NEW-FITNESS = FUNCTION EXP(-CURRENT-SOLUTION / 1000.00)
           COMPUTE NEW-FITNESS = NEW-FITNESS + (FUNCTION RANDOM(10) / 1000.00)
           COMPUTE DELTA-FITNESS = NEW-FITNESS - CURRENT-FITNESS.

       ACCEPT-OR-REJECT.
           IF DELTA-FITNESS < 0
               MOVE NEW-SOLUTION TO CURRENT-SOLUTION
               MOVE NEW-FITNESS TO CURRENT-FITNESS
               IF NEW-FITNESS < BEST-FITNESS
                   MOVE NEW-SOLUTION TO BEST-SOLUTION
                   MOVE NEW-FITNESS TO BEST-FITNESS
               END-IF
           ELSE
               COMPUTE ACCEPT-PROB = FUNCTION EXP(-DELTA-FITNESS / CURRENT-TEMP)
               COMPUTE RAND-VALUE = FUNCTION RANDOM(10000) / 10000.00
               IF RAND-VALUE < ACCEPT-PROB
                   MOVE NEW-SOLUTION TO CURRENT-SOLUTION
                   MOVE NEW-FITNESS TO CURRENT-FITNESS
               END-IF
           END-IF.

       COOL-TEMPERATURE.
           COMPUTE CURRENT-TEMP = CURRENT-TEMP * COOLING-RATE.

       WRITE-RESULTS.
           OPEN OUTPUT OUTPUT-FILE
           MOVE "SIMULATED ANNEALING RESULTS" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "BEST SOLUTION: " TO OUTPUT-RECORD(1:15)
           MOVE BEST-SOLUTION TO OUTPUT-RECORD(16:4)
           MOVE " BEST FITNESS: " TO OUTPUT-RECORD(20:13)
           MOVE BEST-FITNESS TO OUTPUT-RECORD(33:8)
           WRITE OUTPUT-RECORD
           MOVE "FINAL TEMPERATURE: " TO OUTPUT-RECORD(1:17)
           MOVE CURRENT-TEMP TO OUTPUT-RECORD(18:8)
           WRITE OUTPUT-RECORD
           CLOSE OUTPUT-FILE.

       END PROGRAM SIMULATED-ANNEALING.
```

## Explanation of Key Components:

### 1. **Initialization Section**
- Sets up initial temperature, cooling rate, and solution parameters
- Generates random starting solutions

### 2. **Main Loop Structure**
- Continues until temperature drops below final threshold
- Performs neighbor generation and fitness evaluation
- Applies acceptance probability for worse solutions

### 3. **Key Algorithm Elements**

**Temperature Cooling:**
```cobol
COMPUTE CURRENT-TEMP = CURRENT-TEMP * COOLING-RATE
```

**Acceptance Probability:**
```cobol
COMPUTE ACCEPT-PROB = FUNCTION EXP(-DELTA-FITNESS / CURRENT-TEMP)
```

**Random Number Generation:**
```cobol
COMPUTE RAND-VALUE = FUNCTION RANDOM(10000) / 10000.00
```

### 4. **Output Generation**
- Records the best solution found
- Saves results to a text file

This COBOL implementation demonstrates the core principles of simulated annealing: probabilistic acceptance of worse solutions to escape local optima, gradual temperature reduction, and iterative improvement toward an optimal solution.