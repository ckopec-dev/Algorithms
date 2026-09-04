# Binary Exponentiation Algorithm in COBOL

Here's an example implementation of binary exponentiation (also known as exponentiation by squaring) in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BINARY-EXPONENTIATION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BASE              PIC 9(10) VALUE 2.
       01 EXPONENT          PIC 9(10) VALUE 10.
       01 RESULT            PIC 9(20) VALUE 1.
       01 TEMP-EXPONENT     PIC 9(10).
       01 TEMP-BASE         PIC 9(10).
       01 BIT-VALUE         PIC 9.
       01 TEMP-RESULT       PIC 9(20).

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Computing " BASE " raised to the power of " EXPONENT
           PERFORM BINARY-EXPONENTIATION
           DISPLAY "Result: " RESULT
           STOP RUN.

       BINARY-EXPONENTIATION.
           MOVE 1 TO RESULT
           MOVE EXPONENT TO TEMP-EXPONENT
           MOVE BASE TO TEMP-BASE

           PERFORM UNTIL TEMP-EXPONENT = 0
               IF TEMP-EXPONENT IS EVEN
                   COMPUTE TEMP-BASE = TEMP-BASE * TEMP-BASE
                   COMPUTE TEMP-EXPONENT = TEMP-EXPONENT / 2
               ELSE
                   COMPUTE RESULT = RESULT * TEMP-BASE
                   COMPUTE TEMP-EXPONENT = TEMP-EXPONENT - 1
               END-IF
           END-PERFORM.

       BINARY-EXPONENTIATION-V2.
           MOVE 1 TO RESULT
           MOVE EXPONENT TO TEMP-EXPONENT
           MOVE BASE TO TEMP-BASE

           PERFORM UNTIL TEMP-EXPONENT = 0
               IF TEMP-EXPONENT IS EVEN
                   COMPUTE TEMP-BASE = TEMP-BASE * TEMP-BASE
                   COMPUTE TEMP-EXPONENT = TEMP-EXPONENT / 2
               ELSE
                   COMPUTE RESULT = RESULT * TEMP-BASE
                   COMPUTE TEMP-EXPONENT = TEMP-EXPONENT - 1
               END-IF
           END-PERFORM.

       BINARY-EXPONENTIATION-RECURSIVE.
           IF EXPONENT = 0
               MOVE 1 TO RESULT
               GO TO RETURN-RESULT
           ELSE IF EXPONENT = 1
               MOVE BASE TO RESULT
               GO TO RETURN-RESULT
           ELSE IF EXPONENT IS EVEN
               COMPUTE TEMP-EXPONENT = EXPONENT / 2
               PERFORM BINARY-EXPONENTIATION-RECURSIVE
               COMPUTE RESULT = RESULT * RESULT
               GO TO RETURN-RESULT
           ELSE
               COMPUTE TEMP-EXPONENT = EXPONENT - 1
               PERFORM BINARY-EXPONENTIATION-RECURSIVE
               COMPUTE RESULT = RESULT * BASE
               GO TO RETURN-RESULT
           END-IF.

       RETURN-RESULT.
           EXIT.
```

## Alternative Simpler Version

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLE-BINARY-EXP.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BASE              PIC 9(5) VALUE 3.
       01 EXPONENT          PIC 9(5) VALUE 4.
       01 RESULT            PIC 9(10) VALUE 1.
       01 TEMP-EXP          PIC 9(5).
       01 TEMP-BASE         PIC 9(5).

       PROCEDURE DIVISION.
       COMPUTE-EXPONENTIATION.
           DISPLAY "Base: " BASE
           DISPLAY "Exponent: " EXPONENT
           
           MOVE 1 TO RESULT
           MOVE EXPONENT TO TEMP-EXP
           MOVE BASE TO TEMP-BASE

           PERFORM UNTIL TEMP-EXP = 0
               IF TEMP-EXP IS NOT EVEN
                   COMPUTE RESULT = RESULT * TEMP-BASE
               END-IF
               COMPUTE TEMP-BASE = TEMP-BASE * TEMP-BASE
               COMPUTE TEMP-EXP = TEMP-EXP / 2
           END-PERFORM

           DISPLAY "Result: " RESULT
           STOP RUN.
```

## How It Works:

1. **Binary Exponentiation Algorithm** uses the principle that:
   - If exponent is even: `a^n = (a^(n/2))^2`
   - If exponent is odd: `a^n = a * a^(n-1)`

2. **Steps**:
   - Initialize result to 1
   - While exponent > 0:
     - If exponent is odd, multiply result by base
     - Square the base and halve the exponent
   - Return the result

3. **Time Complexity**: O(log n) instead of O(n)

## Example Output:
```
Computing 2 raised to the power of 10
Result: 1024
```

This implementation efficiently computes large powers by reducing the number of multiplications needed from O(n) to O(log n).