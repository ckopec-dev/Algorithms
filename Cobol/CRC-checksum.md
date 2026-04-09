# CRC Checksum Algorithm in COBOL

Here's an example of a CRC-8 checksum algorithm implemented in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRC-CHECKSUM.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-DATA-ARRAY.
           05  WS-DATA-BYTE    PIC 999    VALUE 0.
           05  WS-DATA-BYTE    PIC 999    VALUE 0.
           05  WS-DATA-BYTE    PIC 999    VALUE 0.
           05  WS-DATA-BYTE    PIC 999    VALUE 0.
           05  WS-DATA-BYTE    PIC 999    VALUE 0.
           05  WS-DATA-BYTE    PIC 999    VALUE 0.
           05  WS-DATA-BYTE    PIC 999    VALUE 0.
           05  WS-DATA-BYTE    PIC 999    VALUE 0.
           05  WS-DATA-BYTE    PIC 999    VALUE 0.
           05  WS-DATA-BYTE    PIC 999    VALUE 0.

       01  WS-CRC-RESULT         PIC 999    VALUE 0.
       01  WS-TEMP               PIC 999    VALUE 0.
       01  WS-INDEX              PIC 99     VALUE 1.
       01  WS-BIT-INDEX          PIC 99     VALUE 1.
       01  WS-INPUT-BYTE         PIC 999    VALUE 0.
       01  WS-CRC-POLYNOMIAL     PIC 999    VALUE 133.
       01  WS-CRC-INITIAL        PIC 999    VALUE 0.
       01  WS-CRC-RESULT-TEMP    PIC 999    VALUE 0.

       01  WS-POLYNOMIAL-TABLE.
           05  WS-POLY-ENTRY     PIC 999    OCCURS 256 TIMES.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-POLYNOMIAL-TABLE
           PERFORM CALCULATE-CRC
           DISPLAY "CRC Result: " WS-CRC-RESULT
           STOP RUN.

       INITIALIZE-POLYNOMIAL-TABLE.
           MOVE 0 TO WS-INDEX
           PERFORM VARYING WS-INDEX FROM 0 BY 1
               UNTIL WS-INDEX > 255
               COMPUTE WS-POLY-ENTRY(WS-INDEX + 1) = 
                   FUNCTION REMAINDER(WS-INDEX * 256, 133)
           END-PERFORM.

       CALCULATE-CRC.
           MOVE WS-CRC-INITIAL TO WS-CRC-RESULT
           MOVE 1 TO WS-INDEX

           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > 10
               MOVE WS-DATA-BYTE(WS-INDEX) TO WS-INPUT-BYTE
               PERFORM UPDATE-CRC
           END-PERFORM.

       UPDATE-CRC.
           COMPUTE WS-CRC-RESULT = WS-CRC-RESULT XOR WS-INPUT-BYTE
           MOVE WS-CRC-RESULT TO WS-TEMP
           MOVE 0 TO WS-BIT-INDEX

           PERFORM VARYING WS-BIT-INDEX FROM 1 BY 1
               UNTIL WS-BIT-INDEX > 8
               IF WS-TEMP >= 128
                   COMPUTE WS-TEMP = FUNCTION REMAINDER(WS-TEMP * 2, 133)
                   COMPUTE WS-CRC-RESULT = WS-CRC-RESULT XOR 1
               ELSE
                   COMPUTE WS-TEMP = FUNCTION REMAINDER(WS-TEMP * 2, 133)
               END-IF
               COMPUTE WS-BIT-INDEX = WS-BIT-INDEX + 1
           END-PERFORM.

       END PROGRAM CRC-CHECKSUM.
```

## Alternative Simpler CRC-8 Implementation

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLE-CRC8.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-DATA-INPUT.
           05  WS-DATA-BYTE    PIC 999    VALUE 0.
           05  WS-DATA-BYTE    PIC 999    VALUE 0.
           05  WS-DATA-BYTE    PIC 999    VALUE 0.
           05  WS-DATA-BYTE    PIC 999    VALUE 0.
           05  WS-DATA-BYTE    PIC 999    VALUE 0.

       01  WS-CRC-RESULT         PIC 999    VALUE 0.
       01  WS-INDEX              PIC 99     VALUE 1.
       01  WS-TEMP               PIC 999    VALUE 0.
       01  WS-BYTE-VALUE         PIC 999    VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           MOVE 0 TO WS-CRC-RESULT
           PERFORM CALCULATE-CRC-8
           DISPLAY "CRC-8 Result: " WS-CRC-RESULT
           STOP RUN.

       CALCULATE-CRC-8.
           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > 5
               MOVE WS-DATA-BYTE(WS-INDEX) TO WS-BYTE-VALUE
               PERFORM UPDATE-CRC-8
           END-PERFORM.

       UPDATE-CRC-8.
           COMPUTE WS-CRC-RESULT = WS-CRC-RESULT XOR WS-BYTE-VALUE
           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > 8
               IF WS-CRC-RESULT >= 128
                   COMPUTE WS-CRC-RESULT = FUNCTION REMAINDER(
                       (WS-CRC-RESULT * 2), 256)
                   COMPUTE WS-CRC-RESULT = WS-CRC-RESULT XOR 1
               ELSE
                   COMPUTE WS-CRC-RESULT = FUNCTION REMAINDER(
                       (WS-CRC-RESULT * 2), 256)
               END-IF
               ADD 1 TO WS-INDEX
           END-PERFORM.

       END PROGRAM SIMPLE-CRC8.
```

## Key Features of This COBOL CRC Implementation:

1. **Data Structure**: Uses working-storage sections to define data arrays and variables
2. **Loop Control**: Implements DO loops with varying indexes for data processing
3. **Bit Manipulation**: Uses XOR operations and bit shifting for CRC calculation
4. **Polynomial Division**: Implements standard CRC polynomial division
5. **Modular Design**: Separates initialization, calculation, and update functions

## Notes:
- This is a simplified CRC-8 implementation
- The polynomial value (133) can be changed for different CRC standards
- Input data should be loaded into the WS-DATA-BYTE array before processing
- The algorithm processes each byte sequentially and updates the CRC value
- Results are displayed at the end of execution

