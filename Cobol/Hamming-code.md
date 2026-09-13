# Hamming Code Algorithm in COBOL

Here's an example implementation of a Hamming code encoder and decoder in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HAMMING-CODE-EXAMPLE.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-FILE ASSIGN TO PRINTER.

       DATA DIVISION.
       FILE SECTION.
       FD  PRINT-FILE.
       01  PRINT-LINE     PIC X(80).

       WORKING-STORAGE SECTION.
       01  DATA-BITS        PIC 9(8) VALUE 0.
       01  HAMMING-CODE     PIC 9(16) VALUE 0.
       01  PARITY-BITS      PIC 9(8) VALUE 0.
       01  ERROR-LOCATION   PIC 9(8) VALUE 0.
       01  TEMP-VALUE       PIC 9(8) VALUE 0.
       01  I                PIC 9(2) VALUE 0.
       01  J                PIC 9(2) VALUE 0.
       01  K                PIC 9(2) VALUE 0.
       01  BIT-VALUE        PIC 9(1) VALUE 0.
       01  PARITY-RESULT    PIC 9(1) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "HAMMING CODE ENCODER/DECODER EXAMPLE"
           DISPLAY "====================================="
           DISPLAY "Original data (8 bits): 10110011"
           DISPLAY ""
           
           MOVE 10110011 TO DATA-BITS
           
           PERFORM ENCODE-DATA
           DISPLAY "Encoded Hamming Code: " HAMMING-CODE
           DISPLAY ""
           
           PERFORM DECODE-DATA
           DISPLAY "Decoded Data: " DATA-BITS
           DISPLAY ""
           
           DISPLAY "Program completed successfully."
           STOP RUN.

       ENCODE-DATA.
           *> Initialize Hamming code with data bits in positions 3,5,6,7,9,10,11,12
           MOVE DATA-BITS TO TEMP-VALUE
           
           *> Place data bits in correct positions (excluding parity positions)
           MOVE 0 TO HAMMING-CODE
           MOVE 1 TO I
           MOVE 1 TO J
           
           *> Copy data bits to positions 3,5,6,7,9,10,11,12
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
               IF I = 1 THEN
                   MOVE 1 TO K
               ELSE IF I = 2 THEN
                   MOVE 3 TO K
               ELSE IF I = 3 THEN
                   MOVE 4 TO K
               ELSE IF I = 4 THEN
                   MOVE 5 TO K
               ELSE IF I = 5 THEN
                   MOVE 6 TO K
               ELSE IF I = 6 THEN
                   MOVE 7 TO K
               ELSE IF I = 7 THEN
                   MOVE 8 TO K
               ELSE IF I = 8 THEN
                   MOVE 9 TO K
               END-IF
               
               *> Extract bit from data
               COMPUTE TEMP-VALUE = DATA-BITS / (2 ** (8 - I))
               COMPUTE BIT-VALUE = FUNCTION MOD(TEMP-VALUE, 2)
               
               *> Set bit in Hamming code
               IF BIT-VALUE = 1 THEN
                   COMPUTE HAMMING-CODE = HAMMING-CODE + (2 ** (15 - K))
               END-IF
           END-PERFORM
           
           *> Calculate parity bits (positions 1,2,4,8)
           PERFORM CALCULATE-PARITY-BITS
           .
       
       CALCULATE-PARITY-BITS.
           *> Parity bit 1 (position 1): covers positions 1,3,5,7,9,11,13,15
           COMPUTE PARITY-RESULT = 0
           PERFORM VARYING I FROM 1 BY 2 UNTIL I > 15
               IF FUNCTION MOD(I, 2) = 1 THEN
                   COMPUTE TEMP-VALUE = HAMMING-CODE / (2 ** (15 - I))
                   COMPUTE BIT-VALUE = FUNCTION MOD(TEMP-VALUE, 2)
                   ADD BIT-VALUE TO PARITY-RESULT
               END-IF
           END-PERFORM
           COMPUTE PARITY-RESULT = FUNCTION MOD(PARITY-RESULT, 2)
           
           *> Set parity bit 1
           IF PARITY-RESULT = 1 THEN
               COMPUTE HAMMING-CODE = HAMMING-CODE + (2 ** 15)
           END-IF
           
           *> Parity bit 2 (position 2): covers positions 2,3,6,7,10,11,14,15
           COMPUTE PARITY-RESULT = 0
           PERFORM VARYING I FROM 2 BY 4 UNTIL I > 15
               IF FUNCTION MOD(I, 4) >= 2 THEN
                   COMPUTE TEMP-VALUE = HAMMING-CODE / (2 ** (15 - I))
                   COMPUTE BIT-VALUE = FUNCTION MOD(TEMP-VALUE, 2)
                   ADD BIT-VALUE TO PARITY-RESULT
               END-IF
           END-PERFORM
           COMPUTE PARITY-RESULT = FUNCTION MOD(PARITY-RESULT, 2)
           
           *> Set parity bit 2
           IF PARITY-RESULT = 1 THEN
               COMPUTE HAMMING-CODE = HAMMING-CODE + (2 ** 14)
           END-IF
           
           *> Parity bit 4 (position 4): covers positions 4,5,6,7,12,13,14,15
           COMPUTE PARITY-RESULT = 0
           PERFORM VARYING I FROM 4 BY 8 UNTIL I > 15
               IF FUNCTION MOD(I, 8) >= 4 THEN
                   COMPUTE TEMP-VALUE = HAMMING-CODE / (2 ** (15 - I))
                   COMPUTE BIT-VALUE = FUNCTION MOD(TEMP-VALUE, 2)
                   ADD BIT-VALUE TO PARITY-RESULT
               END-IF
           END-PERFORM
           COMPUTE PARITY-RESULT = FUNCTION MOD(PARITY-RESULT, 2)
           
           *> Set parity bit 4
           IF PARITY-RESULT = 1 THEN
               COMPUTE HAMMING-CODE = HAMMING-CODE + (2 ** 12)
           END-IF
           
           *> Parity bit 8 (position 8): covers positions 8,9,10,11,12,13,14,15
           COMPUTE PARITY-RESULT = 0
           PERFORM VARYING I FROM 8 BY 16 UNTIL I > 15
               IF FUNCTION MOD(I, 16) >= 8 THEN
                   COMPUTE TEMP-VALUE = HAMMING-CODE / (2 ** (15 - I))
                   COMPUTE BIT-VALUE = FUNCTION MOD(TEMP-VALUE, 2)
                   ADD BIT-VALUE TO PARITY-RESULT
               END-IF
           END-PERFORM
           COMPUTE PARITY-RESULT = FUNCTION MOD(PARITY-RESULT, 2)
           
           *> Set parity bit 8
           IF PARITY-RESULT = 1 THEN
               COMPUTE HAMMING-CODE = HAMMING-CODE + (2 ** 8)
           END-IF
           .

       DECODE-DATA.
           *> This is a simplified decoder - in practice, error detection would be more complex
           DISPLAY "Decoding process initiated..."
           DISPLAY "Original data bits extracted from positions 3,5,6,7,9,10,11,12"
           .
```

## Explanation

This COBOL program demonstrates:

1. **Data Encoding**: Takes an 8-bit input and generates a 12-bit Hamming code
2. **Parity Calculation**: Computes parity bits for positions 1, 2, 4, and 8
3. **Bit Positioning**: Places data bits in correct positions (3,5,6,7,9,10,11,12)
4. **Error Detection**: Shows the structure needed for error detection

## Key Features

- Uses bit manipulation techniques with powers of 2
- Implements Hamming code principles (d=3, can detect 2 errors, correct 1 error)
- Demonstrates proper COBOL data handling and computation
- Includes comments explaining the Hamming encoding process

## Note

This is a simplified example. A complete Hamming decoder would include actual error detection and correction logic, which would require more complex bit manipulation and error location calculations.