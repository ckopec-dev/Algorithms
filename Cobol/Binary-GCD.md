# Binary GCD Algorithm in COBOL

Here's an implementation of the Binary GCD (Stein's Algorithm) in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BINARY-GCD.
       AUTHOR. COBOL EXAMPLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  NUM1               PIC 9(10) VALUE 48.
       01  NUM2               PIC 9(10) VALUE 18.
       01  TEMP               PIC 9(10).
       01  SHIFT              PIC 9(10) VALUE 0.
       01  RESULT             PIC 9(10).
       01  TEMP1              PIC 9(10).
       01  TEMP2              PIC 9(10).

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Finding GCD of " NUM1 " and " NUM2
           MOVE NUM1 TO TEMP1
           MOVE NUM2 TO TEMP2
           
           PERFORM CALCULATE-GCD
           
           DISPLAY "GCD is: " RESULT
           STOP RUN.

       CALCULATE-GCD.
           IF TEMP1 = 0
               MOVE TEMP2 TO RESULT
               GO TO RETURN-GCD
           END-IF
           
           IF TEMP2 = 0
               MOVE TEMP1 TO RESULT
               GO TO RETURN-GCD
           END-IF
           
           MOVE 0 TO SHIFT
           
           PERFORM UNTIL (TEMP1 = 0 OR TEMP2 = 0)
               IF (TEMP1 AND 1) = 0
                   DIVIDE TEMP1 BY 2 GIVING TEMP1
                   ADD 1 TO SHIFT
               ELSE IF (TEMP2 AND 1) = 0
                   DIVIDE TEMP2 BY 2 GIVING TEMP2
                   ADD 1 TO SHIFT
               ELSE IF TEMP1 > TEMP2
                   SUBTRACT TEMP2 FROM TEMP1 GIVING TEMP1
               ELSE
                   SUBTRACT TEMP1 FROM TEMP2 GIVING TEMP2
               END-IF
           END-PERFORM
           
           IF TEMP1 = 0
               MOVE TEMP2 TO RESULT
           ELSE
               MOVE TEMP1 TO RESULT
           END-IF
           
           MULTIPLY RESULT BY 2 BY SHIFT GIVING RESULT.

       RETURN-GCD.
           EXIT.
```

## Alternative Implementation (More Readable)

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BINARY-GCD-ALT.
       AUTHOR. COBOL EXAMPLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  A                  PIC 9(10) VALUE 48.
       01  B                  PIC 9(10) VALUE 18.
       01  GCD                PIC 9(10).
       01  SHIFT              PIC 9(10) VALUE 0.
       01  TEMP-A             PIC 9(10).
       01  TEMP-B             PIC 9(10).

       PROCEDURE DIVISION.
       BINARY-GCD-MAIN.
           DISPLAY "Binary GCD Algorithm"
           DISPLAY "Numbers: " A " and " B
           
           MOVE A TO TEMP-A
           MOVE B TO TEMP-B
           
           PERFORM BINARY-GCD-ALGORITHM
           
           DISPLAY "GCD = " GCD
           STOP RUN.

       BINARY-GCD-ALGORITHM.
           IF TEMP-A = 0
               MOVE TEMP-B TO GCD
               GO TO END-ALGORITHM
           END-IF
           
           IF TEMP-B = 0
               MOVE TEMP-A TO GCD
               GO TO END-ALGORITHM
           END-IF
           
           PERFORM COUNT-TRAILING-ZEROS
           
           PERFORM UNTIL (TEMP-A = 0 OR TEMP-B = 0)
               PERFORM REMOVE-TRAILING-ZEROS
               IF TEMP-A > TEMP-B
                   SUBTRACT TEMP-B FROM TEMP-A GIVING TEMP-A
               ELSE
                   SUBTRACT TEMP-A FROM TEMP-B GIVING TEMP-B
               END-IF
           END-PERFORM
           
           IF TEMP-A = 0
               MOVE TEMP-B TO GCD
           ELSE
               MOVE TEMP-A TO GCD
           END-IF
           
           MULTIPLY GCD BY 2 BY SHIFT GIVING GCD.

       COUNT-TRAILING-ZEROS.
           MOVE 0 TO SHIFT
           PERFORM UNTIL (TEMP-A = 0 OR TEMP-B = 0)
               IF (TEMP-A AND 1) = 0
                   DIVIDE TEMP-A BY 2 GIVING TEMP-A
                   ADD 1 TO SHIFT
               ELSE IF (TEMP-B AND 1) = 0
                   DIVIDE TEMP-B BY 2 GIVING TEMP-B
                   ADD 1 TO SHIFT
               ELSE
                   GO TO END-COUNT
               END-IF
           END-PERFORM.
       
       END-COUNT.
           EXIT.

       REMOVE-TRAILING-ZEROS.
           IF (TEMP-A AND 1) = 0
               DIVIDE TEMP-A BY 2 GIVING TEMP-A
           END-IF
           IF (TEMP-B AND 1) = 0
               DIVIDE TEMP-B BY 2 GIVING TEMP-B
           END-IF.

       END-ALGORITHM.
           EXIT.
```

## Key Features of This Implementation:

1. **Binary GCD Algorithm**: Uses bit operations instead of division
2. **Efficiency**: Reduces the problem size by half in each iteration
3. **Bit Manipulation**: Uses AND operation to check for even/odd numbers
4. **Shift Operations**: Uses multiplication by powers of 2 for final result
5. **Loop Control**: Uses WHILE-style loop with conditional termination

## Algorithm Steps:
1. If either number is 0, return the other number
2. Count common factors of 2
3. Remove factors of 2 from both numbers
4. Subtract the smaller from the larger
5. Repeat until one number becomes 0
6. Multiply result by 2^shift to account for common factors

This implementation demonstrates the binary GCD algorithm's efficiency in COBOL, which is particularly useful for systems where division operations are expensive.

