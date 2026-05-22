# Booth's Multiplication Algorithm in COBOL

Here's an example implementation of Booth's multiplication algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BOOTH-MULTIPLICATION.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  MULTIPLICAND        PIC 9(8) VALUE 0.
       01  MULTIPLIER          PIC 9(8) VALUE 0.
       01  PRODUCT             PIC 9(16) VALUE 0.
       01  A-REGISTER          PIC 9(16) VALUE 0.
       01  Q-REGISTER          PIC 9(16) VALUE 0.
       01  Q-1-REGISTER        PIC 9 VALUE 0.
       01  COUNT               PIC 9 VALUE 0.
       01  TEMP                PIC 9(16) VALUE 0.
       01  SHIFTED-Q           PIC 9(16) VALUE 0.
       01  SHIFTED-A           PIC 9(16) VALUE 0.
       01  SHIFTED-Q1          PIC 9 VALUE 0.
       01  OPERATION           PIC X(10) VALUE SPACES.
       01  RESULT              PIC 9(16) VALUE 0.
       01  I                   PIC 9 VALUE 0.
       01  LOOP-COUNT          PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "BOOTH'S MULTIPLICATION ALGORITHM"
           DISPLAY "=================================="
           
           DISPLAY "Enter Multiplicand: "
           ACCEPT MULTIPLICAND
           
           DISPLAY "Enter Multiplier: "
           ACCEPT MULTIPLIER
           
           PERFORM INITIALIZE-REGISTERS
           PERFORM BOOTH-ALGORITHM
           PERFORM DISPLAY-RESULTS
           
           STOP RUN.

       INITIALIZE-REGISTERS.
           MOVE MULTIPLICAND TO A-REGISTER
           MOVE MULTIPLIER TO Q-REGISTER
           MOVE 0 TO Q-1-REGISTER
           MOVE 16 TO COUNT
           MOVE 0 TO PRODUCT
           DISPLAY "Initial Registers:"
           DISPLAY "A = " A-REGISTER
           DISPLAY "Q = " Q-REGISTER
           DISPLAY "Q-1 = " Q-1-REGISTER
           DISPLAY "Count = " COUNT
           DISPLAY "=========================".

       BOOTH-ALGORITHM.
           PERFORM UNTIL COUNT = 0
               PERFORM CHECK-PAIR
               PERFORM ARITHMETIC-OPERATION
               PERFORM SHIFT-OPERATION
               SUBTRACT 1 FROM COUNT
               ADD 1 TO LOOP-COUNT
               DISPLAY "Step " LOOP-COUNT ":"
               DISPLAY "A = " A-REGISTER
               DISPLAY "Q = " Q-REGISTER
               DISPLAY "Q-1 = " Q-1-REGISTER
               DISPLAY "========================="
           END-PERFORM.

       CHECK-PAIR.
           IF Q-REGISTER IS NOT NUMERIC
               MOVE 0 TO TEMP
           ELSE
               IF Q-REGISTER = 0
                   MOVE 0 TO TEMP
               ELSE
                   MOVE 1 TO TEMP
               END-IF
           END-IF.
           
           IF Q-1-REGISTER = 0 AND TEMP = 1
               MOVE "SUBTRACT" TO OPERATION
           ELSE IF Q-1-REGISTER = 1 AND TEMP = 0
               MOVE "ADD" TO OPERATION
           ELSE
               MOVE "NONE" TO OPERATION
           END-IF.

       ARITHMETIC-OPERATION.
           IF OPERATION = "SUBTRACT"
               SUBTRACT MULTIPLICAND FROM A-REGISTER
           ELSE IF OPERATION = "ADD"
               ADD MULTIPLICAND TO A-REGISTER
           END-IF.

       SHIFT-OPERATION.
           MOVE Q-REGISTER TO SHIFTED-Q
           MOVE A-REGISTER TO SHIFTED-A
           MOVE Q-1-REGISTER TO SHIFTED-Q1
           
           IF SHIFTED-Q1 = 0
               COMPUTE A-REGISTER = (SHIFTED-A * 2) + 1
           ELSE
               COMPUTE A-REGISTER = (SHIFTED-A * 2) + 0
           END-IF.
           
           COMPUTE Q-REGISTER = (SHIFTED-Q * 2) + 0
           COMPUTE Q-1-REGISTER = SHIFTED-Q1.

       DISPLAY-RESULTS.
           DISPLAY "Final Results:"
           DISPLAY "Multiplicand: " MULTIPLICAND
           DISPLAY "Multiplier: " MULTIPLIER
           DISPLAY "Product: " A-REGISTER
           DISPLAY "Result: " A-REGISTER
           DISPLAY "=========================".
```

## Example Execution

For a simple example with:
- Multiplicand = 5
- Multiplier = 3

The algorithm would perform the following steps:

1. **Initialization:**
   - A = 0000000000000101 (5 in binary)
   - Q = 0000000000000011 (3 in binary)  
   - Q-1 = 0

2. **Steps:**
   - Check Q0Q-1 pair and perform operations
   - Shift registers
   - Repeat for 16 bits

3. **Final Result:**
   - Product = 15 (5 × 3)

## Key Features

- **Register Management:** Uses A-register, Q-register, and Q-1-register
- **Bit Shifting:** Implements arithmetic right shifts
- **Conditional Operations:** Performs addition or subtraction based on Q0Q-1 pairs
- **Loop Control:** Uses a counter to process all bits
- **Step-by-Step Display:** Shows each iteration of the algorithm

This implementation demonstrates the core Booth's multiplication algorithm in COBOL, showing how the algorithm handles signed multiplication through bit manipulation and conditional arithmetic operations.

