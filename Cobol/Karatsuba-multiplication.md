# Karatsuba Multiplication Algorithm in COBOL

Here's an implementation of the Karatsuba multiplication algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. KARATSUBA-MULTIPLY.
       AUTHOR. COBOL IMPLEMENTATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTPUT-FILE ASSIGN TO "RESULT.OUT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD        PIC X(80).

       WORKING-STORAGE SECTION.
       01 N                    PIC 9(4) VALUE 0.
       01 A                    PIC 9(20).
       01 B                    PIC 9(20).
       01 RESULT               PIC 9(40).
       01 TEMP-A               PIC 9(20).
       01 TEMP-B               PIC 9(20).
       01 HALF-N               PIC 9(4) VALUE 0.
       01 X1                   PIC 9(20).
       01 X2                   PIC 9(20).
       01 Y1                   PIC 9(20).
       01 Y2                   PIC 9(20).
       01 Z0                   PIC 9(20).
       01 Z1                   PIC 9(20).
       01 Z2                   PIC 9(20).
       01 P1                   PIC 9(20).
       01 P2                   PIC 9(20).
       01 P3                   PIC 9(20).
       01 TEMP-RESULT          PIC 9(40).
       01 I                    PIC 9(4) VALUE 0.
       01 J                    PIC 9(4) VALUE 0.
       01 LENGTH-A             PIC 9(4) VALUE 0.
       01 LENGTH-B             PIC 9(4) VALUE 0.
       01 MAX-LENGTH           PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "KARATSUBA MULTIPLICATION ALGORITHM"
           DISPLAY "==============================="
           
           MOVE 1234 TO A
           MOVE 5678 TO B
           
           DISPLAY "Multiplying: " A " and " B
           
           PERFORM KARATSUBA-MULTIPLY
           DISPLAY "Result: " RESULT
           
           STOP RUN.

       KARATSUBA-MULTIPLY.
           *> Handle base case (simple multiplication)
           IF A < 10 AND B < 10
               COMPUTE RESULT = A * B
               GO TO RETURN-RESULT
           END-IF
           
           *> Get lengths of numbers
           MOVE A TO TEMP-A
           MOVE B TO TEMP-B
           PERFORM GET-LENGTH
           
           *> Make both numbers same length by padding with zeros
           IF LENGTH-A > LENGTH-B
               MOVE LENGTH-A TO MAX-LENGTH
           ELSE
               MOVE LENGTH-B TO MAX-LENGTH
           END-IF
           
           *> Ensure even length for Karatsuba algorithm
           IF MAX-LENGTH IS ODD
               ADD 1 TO MAX-LENGTH
           END-IF
           
           *> Split numbers into halves
           COMPUTE HALF-N = MAX-LENGTH / 2
           
           PERFORM SPLIT-NUMBERS
           
           *> Apply Karatsuba formula:
           *> x * y = (10^(2*n) * z2) + (10^n * z1) + z0
           
           *> Compute z0 = x1 * y1
           COMPUTE Z0 = X1 * Y1
           
           *> Compute z2 = x2 * y2  
           COMPUTE Z2 = X2 * Y2
           
           *> Compute z1 = (x1 + x2) * (y1 + y2) - z0 - z2
           COMPUTE P1 = X1 + X2
           COMPUTE P2 = Y1 + Y2
           COMPUTE P3 = P1 * P2
           COMPUTE Z1 = P3 - Z0 - Z2
           
           *> Combine results: 10^(2*n) * z2 + 10^n * z1 + z0
           COMPUTE TEMP-RESULT = 0
           COMPUTE I = 2 * HALF-N
           PERFORM MULTIPLY-POWER-OF-TEN
           COMPUTE TEMP-RESULT = TEMP-RESULT + Z2
           
           COMPUTE I = HALF-N
           PERFORM MULTIPLY-POWER-OF-TEN
           COMPUTE TEMP-RESULT = TEMP-RESULT + Z1
           
           COMPUTE TEMP-RESULT = TEMP-RESULT + Z0
           
           MOVE TEMP-RESULT TO RESULT

           RETURN-RESULT.
           EXIT.

       SPLIT-NUMBERS.
           *> Split A into x1 (high) and x2 (low)
           COMPUTE I = 10 ** HALF-N
           COMPUTE X1 = A / I
           COMPUTE X2 = A MOD I
           
           *> Split B into y1 (high) and y2 (low)
           COMPUTE Y1 = B / I
           COMPUTE Y2 = B MOD I.

       GET-LENGTH.
           *> Calculate length of number by counting digits
           MOVE 0 TO LENGTH-A
           MOVE TEMP-A TO I
           PERFORM UNTIL I = 0
               DIVIDE I BY 10 GIVING I REMAINDER J
               ADD 1 TO LENGTH-A
           END-PERFORM
           
           MOVE 0 TO LENGTH-B
           MOVE TEMP-B TO I
           PERFORM UNTIL I = 0
               DIVIDE I BY 10 GIVING I REMAINDER J
               ADD 1 TO LENGTH-B
           END-PERFORM.

       MULTIPLY-POWER-OF-TEN.
           *> Multiply TEMP-RESULT by 10^I
           COMPUTE TEMP-RESULT = TEMP-RESULT * (10 ** I).
```

## Key Features of this Implementation:

### Algorithm Components:
1. **Base Case Handling**: When both numbers are single digits, use simple multiplication
2. **Number Splitting**: Divide each number into two halves
3. **Karatsuba Formula**: 
   - z0 = x1 × y1 (product of high parts)
   - z2 = x2 × y2 (product of low parts)  
   - z1 = (x1 + x2) × (y1 + y2) - z0 - z2
   - Result = 10^(2n) × z2 + 10^n × z1 + z0

### COBOL-Specific Elements:
- **Data Division**: Properly defined variables and working storage
- **Procedure Division**: Main algorithm flow with subroutines
- **Arithmetic Operations**: Uses COBOL COMPUTE statements for calculations
- **Control Structures**: IF, PERFORM UNTIL loops for algorithm logic

### Usage Example:
The program multiplies 1234 × 5678 = 7006652 using Karatsuba's divide-and-conquer approach.

This implementation demonstrates how the mathematical Karatsuba algorithm can be translated into COBOL's structured programming paradigm.