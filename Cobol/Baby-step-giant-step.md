# Baby-step Giant-step Algorithm in COBOL

The baby-step giant-step algorithm is used to solve the discrete logarithm problem. Here's an implementation in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BABY-STEP-GIANT-STEP.
       AUTHOR. AI Assistant.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
          05 P          PIC 9(10).
          05 G          PIC 9(10).
          05 H          PIC 9(10).

       WORKING-STORAGE SECTION.
       01 WS-TEMP-VALUES.
          05 M          PIC 9(10) VALUE 0.
          05 I          PIC 9(10) VALUE 0.
          05 J          PIC 9(10) VALUE 0.
          05 X          PIC 9(10) VALUE 0.
          05 Y          PIC 9(10) VALUE 0.
          05 TEMP       PIC 9(10) VALUE 0.
          05 FOUND      PIC X VALUE 'N'.
          05 MOD-RESULT PIC 9(10) VALUE 0.
          05 POWER      PIC 9(10) VALUE 1.

       01 WS-MATRIX.
          05 TABLE-BABY.
             10 OCCURS 1000 TIMES.
                15 BABY-STEP-KEY    PIC 9(10).
                15 BABY-STEP-VALUE  PIC 9(10).

       01 WS-OUTPUT.
          05 RESULT     PIC 9(10) VALUE 0.
          05 MSG        PIC X(50) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "Baby-step Giant-step Algorithm"
           DISPLAY "==============================="
           
           PERFORM READ-INPUT
           PERFORM CALCULATE-M
           PERFORM BABY-STEP-PHASE
           PERFORM GIANT-STEP-PHASE
           PERFORM DISPLAY-RESULT
           
           STOP RUN.

       READ-INPUT.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE
               AT END GO TO END-OF-FILE
           END-READ
           CLOSE INPUT-FILE.

       END-OF-FILE.
           DISPLAY "No input data found"
           STOP RUN.

       CALCULATE-M.
           COMPUTE M = FUNCTION SQRT(P) + 1
           DISPLAY "M = " M
           .

       BABY-STEP-PHASE.
           DISPLAY "Executing Baby-step phase..."
           MOVE 0 TO I
           PERFORM VARYING I FROM 0 BY 1 UNTIL I >= M
               COMPUTE TEMP = FUNCTION MOD(G ** I, P)
               MOVE TEMP TO BABY-STEP-KEY(I+1)
               MOVE I TO BABY-STEP-VALUE(I+1)
           END-PERFORM
           .

       GIANT-STEP-PHASE.
           DISPLAY "Executing Giant-step phase..."
           MOVE 1 TO POWER
           COMPUTE POWER = FUNCTION MOD(G ** M, P)
           MOVE 0 TO J
           PERFORM VARYING J FROM 0 BY 1 UNTIL J >= M
               COMPUTE X = FUNCTION MOD(H * (POWER ** J), P)
               PERFORM SEARCH-BABY-TABLE
               IF FOUND = 'Y' THEN
                   COMPUTE RESULT = J * M + I
                   GO TO GIANT-STEP-END
               END-IF
           END-PERFORM
           .

       SEARCH-BABY-TABLE.
           MOVE 'N' TO FOUND
           PERFORM VARYING I FROM 0 BY 1 UNTIL I >= M
               IF BABY-STEP-KEY(I+1) = X
                   MOVE 'Y' TO FOUND
                   EXIT PERFORM
               END-IF
           END-PERFORM
           .

       GIANT-STEP-END.
           .

       DISPLAY-RESULT.
           DISPLAY "Discrete logarithm solution: " RESULT
           IF RESULT = 0
               DISPLAY "No solution found"
           END-IF
           .

       END PROGRAM BABY-STEP-GIANT-STEP.
```

## Example Usage

For the discrete logarithm problem: g^x ≡ h (mod p)

**Input file (input.dat):**
```
17 3 13
```

This represents:
- p = 17 (prime modulus)
- g = 3 (base)
- h = 13 (target value)

**Expected output:**
```
Baby-step Giant-step Algorithm
===============================
M = 5
Executing Baby-step phase...
Executing Giant-step phase...
Discrete logarithm solution: 4
```

This means 3^4 ≡ 13 (mod 17), so x = 4.

## Algorithm Explanation

1. **Baby-step phase**: Compute g^i mod p for i = 0, 1, ..., m-1 and store in a table
2. **Giant-step phase**: Compute h * (g^m)^j mod p for j = 0, 1, ..., m-1
3. **Search**: Look for matches between the two sets
4. **Solution**: If g^i = h * (g^m)^j, then x = j*m + i

**Note**: This COBOL implementation is conceptual and may require compiler-specific extensions for mathematical functions like exponentiation and square root.

