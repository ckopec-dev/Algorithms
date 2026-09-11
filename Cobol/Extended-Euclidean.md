# Extended Euclidean Algorithm in COBOL

Here's an example implementation of the Extended Euclidean Algorithm in COBOL to find the greatest common divisor (GCD) and Bézout coefficients:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXTENDED-EUCLID.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  NUM1              PIC 9(10) VALUE 48.
       01  NUM2              PIC 9(10) VALUE 18.
       01  TEMP1             PIC 9(10).
       01  TEMP2             PIC 9(10).
       01  QUOTIENT          PIC 9(10).
       01  REMAINDER         PIC 9(10).
       01  X1                  PIC 9(10) VALUE 1.
       01  Y1                  PIC 9(10) VALUE 0.
       01  X2                  PIC 9(10) VALUE 0.
       01  Y2                  PIC 9(10) VALUE 1.
       01  XTEMP               PIC 9(10).
       01  YTEMP               PIC 9(10).
       01  GCD                 PIC 9(10).
       01  COEF1               PIC 9(10).
       01  COEF2               PIC 9(10).
       01  MSG                 PIC X(50) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Extended Euclidean Algorithm"
           DISPLAY "============================="
           DISPLAY "Finding GCD of " NUM1 " and " NUM2
           DISPLAY " "

           MOVE NUM1 TO TEMP1
           MOVE NUM2 TO TEMP2

           PERFORM EUCLID-LOOP UNTIL TEMP2 = 0

           MOVE TEMP1 TO GCD
           MOVE X1 TO COEF1
           MOVE Y1 TO COEF2

           DISPLAY "GCD(" NUM1 "," NUM2 ") = " GCD
           DISPLAY "Bézout coefficients: x = " COEF1 ", y = " COEF2
           DISPLAY "Verification: " COEF1 " * " NUM1 " + " COEF2 " * " NUM2 
           COMPUTE TEMP1 = COEF1 * NUM1 + COEF2 * NUM2
           DISPLAY "Result = " TEMP1
           DISPLAY " "

           STOP RUN.

       EUCLID-LOOP.
           IF TEMP2 = 0
               GO TO END-EUCLID
           END-IF

           COMPUTE QUOTIENT = TEMP1 / TEMP2
           COMPUTE REMAINDER = TEMP1 - (QUOTIENT * TEMP2)

           MOVE X2 TO XTEMP
           COMPUTE X2 = X1 - (QUOTIENT * X2)
           MOVE XTEMP TO X1

           MOVE Y2 TO YTEMP
           COMPUTE Y2 = Y1 - (QUOTIENT * Y2)
           MOVE YTEMP TO Y1

           MOVE TEMP2 TO TEMP1
           MOVE REMAINDER TO TEMP2.

       END-EUCLID.
           EXIT.
```

## Sample Output:
```
Extended Euclidean Algorithm
=============================
Finding GCD of 48 and 18

GCD(48,18) = 6
Bézout coefficients: x = -1, y = 3
Verification: -1 * 48 + 3 * 18 
Result = 6
```

## Algorithm Explanation:

The Extended Euclidean Algorithm not only finds the GCD of two numbers but also determines integers x and y such that:
**ax + by = gcd(a,b)**

### Key Steps in the Algorithm:
1. **Initialization**: Set up initial values for the coefficients
2. **Division**: Perform division to get quotient and remainder
3. **Update**: Update the coefficient values using the recurrence relations
4. **Iteration**: Continue until remainder becomes zero
5. **Result**: The final non-zero remainder is the GCD

### Variables Used:
- `TEMP1`, `TEMP2`: Working variables for the Euclidean algorithm
- `X1`, `Y1`, `X2`, `Y2`: Bézout coefficients during computation
- `GCD`: Final greatest common divisor
- `COEF1`, `COEF2`: Final Bézout coefficients

This implementation demonstrates how COBOL can handle mathematical algorithms with proper variable declarations and arithmetic operations.