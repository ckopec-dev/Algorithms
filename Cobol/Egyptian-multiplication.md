# Egyptian Multiplication Algorithm in COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EGYPTIAN-MULTIPLICATION.
       AUTHOR. COBOL EXAMPLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  MULTIPLIER     PIC 9(5) VALUE 17.
       01  MULTIPLICAND   PIC 9(5) VALUE 23.
       01  RESULT         PIC 9(10) VALUE 0.
       01  TEMP-MULTIPLIER PIC 9(5) VALUE 17.
       01  TEMP-MULTIPLICAND PIC 9(5) VALUE 23.
       01  TEMP-RESULT    PIC 9(10) VALUE 0.
       01  REMAINDER      PIC 9(5) VALUE 0.
       01  IS-ODD         PIC X VALUE 'N'.
       01  LOOP-COUNTER   PIC 9(5) VALUE 0.
       01  MAX-LOOPS      PIC 9(5) VALUE 20.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Egyptian Multiplication Algorithm"
           DISPLAY "Multiplying " MULTIPLIER " and " MULTIPLICAND
           DISPLAY "================================"

           MOVE MULTIPLIER TO TEMP-MULTIPLIER
           MOVE MULTIPLICAND TO TEMP-MULTIPLICAND
           MOVE 0 TO RESULT

           PERFORM CALCULATE-RESULT UNTIL TEMP-MULTIPLIER IS LESS THAN 1
           DISPLAY "Result: " RESULT
           STOP RUN.

       CALCULATE-RESULT.
           IF TEMP-MULTIPLIER IS GREATER THAN 0
               COMPUTE REMAINDER = TEMP-MULTIPLIER MOD 2
               IF REMAINDER IS EQUAL TO 1
                   ADD TEMP-MULTIPLICAND TO RESULT
               END-IF
               COMPUTE TEMP-MULTIPLIER = TEMP-MULTIPLIER DIV 2
               COMPUTE TEMP-MULTIPLICAND = TEMP-MULTIPLICAND MULTIPLY 2
           END-IF.

       DISPLAY-STEP-BY-STEP.
           DISPLAY "Step " LOOP-COUNTER
           DISPLAY "  Multiplier: " TEMP-MULTIPLIER
           DISPLAY "  Multiplicand: " TEMP-MULTIPLICAND
           DISPLAY "  Current Result: " RESULT
           ADD 1 TO LOOP-COUNTER
           IF LOOP-COUNTER IS GREATER THAN MAX-LOOPS
               DISPLAY "Maximum iterations reached"
               STOP RUN
           END-IF.
```

## How it works:

This COBOL program implements the Egyptian multiplication algorithm (also known as binary multiplication or Russian peasant multiplication) for multiplying 17 × 23.

### Algorithm Steps:
1. **Initialize**: Start with the two numbers to multiply
2. **Loop**: While the multiplier is greater than 0:
   - If multiplier is odd, add the multiplicand to the result
   - Divide multiplier by 2 (integer division)
   - Multiply multiplicand by 2
3. **Return**: The accumulated result

### Example Execution:
```
Initial: Multiplier = 17, Multiplicand = 23, Result = 0
Step 1: 17 is odd → Add 23 to result → Result = 23
        17 ÷ 2 = 8, 23 × 2 = 46
Step 2: 8 is even → Skip addition
        8 ÷ 2 = 4, 46 × 2 = 92
Step 3: 4 is even → Skip addition
        4 ÷ 2 = 2, 92 × 2 = 184
Step 4: 2 is even → Skip addition
        2 ÷ 2 = 1, 184 × 2 = 368
Step 5: 1 is odd → Add 368 to result → Result = 391
        1 ÷ 2 = 0, 368 × 2 = 736
Final Result: 391
```

The program correctly calculates that 17 × 23 = 391 using the Egyptian multiplication method.

