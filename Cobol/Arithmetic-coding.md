# Arithmetic Coding Algorithm in COBOL

Here's a complete example of an arithmetic coding algorithm implemented in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARITHMETIC-CODING.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "INPUT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "OUTPUT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
          05 INPUT-CHARACTER    PIC X.
          05 FILLER             PIC X(79).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
          05 CODING-RESULT      PIC 9(10).
          05 FILLER             PIC X(70).

       WORKING-STORAGE SECTION.
       01 FREQUENCY-TABLE.
          05 FREQ-ENTRY OCCURS 256 TIMES.
             10 CHAR-VALUE        PIC 9(3) VALUE 0.
             10 CHAR-FREQUENCY    PIC 9(6) VALUE 0.
             10 CHAR-CUMULATIVE   PIC 9(10) VALUE 0.

       01 PROBABILITY-TABLE.
          05 PROB-ENTRY OCCURS 256 TIMES.
             10 CHAR-PROBABILITY  PIC 9(10)V9(10) VALUE 0.0.

       01 INPUT-CHARACTERS.
          05 CHAR-COUNT         PIC 9(6) VALUE 0.
          05 CHAR-ARRAY         OCCURS 1000 TIMES.
             10 INPUT-CHAR        PIC X.

       01 CODING-VARIABLES.
          05 LOWER-BOUND        PIC 9(10)V9(10) VALUE 0.0.
          05 UPPER-BOUND        PIC 9(10)V9(10) VALUE 1.0.
          05 RANGE              PIC 9(10)V9(10) VALUE 1.0.
          05 CODED-VALUE        PIC 9(10)V9(10) VALUE 0.0.
          05 TOTAL-CHARS        PIC 9(6) VALUE 0.
          05 CURRENT-CHAR       PIC X.
          05 CHAR-INDEX         PIC 9(3) VALUE 0.

       01 FILE-CONTROL.
          05 EOF-FLAG           PIC X VALUE 'N'.
             88 END-OF-FILE     VALUE 'Y'.
          05 INPUT-LENGTH       PIC 9(6) VALUE 0.
          05 TOTAL-FREQUENCY    PIC 9(10) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-CODING.
           PERFORM READ-INPUT-DATA.
           PERFORM CALCULATE-FREQUENCIES.
           PERFORM CALCULATE-PROBABILITIES.
           PERFORM ENCODE-TEXT.
           PERFORM WRITE-OUTPUT.
           STOP RUN.

       INITIALIZE-CODING.
           MOVE 0 TO TOTAL-CHARS, TOTAL-FREQUENCY.
           MOVE 'N' TO EOF-FLAG.
           PERFORM INIT-FREQ-TABLE.
           PERFORM INIT-PROB-TABLE.

       INIT-FREQ-TABLE.
           PERFORM VARYING CHAR-INDEX FROM 0 BY 1
               UNTIL CHAR-INDEX > 255
               MOVE CHAR-INDEX TO CHAR-VALUE(CHAR-INDEX)
               MOVE 0 TO CHAR-FREQUENCY(CHAR-INDEX)
               MOVE 0 TO CHAR-CUMULATIVE(CHAR-INDEX)
           END-PERFORM.

       INIT-PROB-TABLE.
           PERFORM VARYING CHAR-INDEX FROM 0 BY 1
               UNTIL CHAR-INDEX > 255
               MOVE 0.0 TO CHAR-PROBABILITY(CHAR-INDEX)
           END-PERFORM.

       READ-INPUT-DATA.
           OPEN INPUT INPUT-FILE.
           PERFORM UNTIL END-OF-FILE
               READ INPUT-FILE INTO INPUT-RECORD
                   AT END MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       ADD 1 TO CHAR-COUNT
                       MOVE INPUT-CHARACTER(1:1) TO CURRENT-CHAR
                       MOVE CURRENT-CHAR TO INPUT-CHAR(CHAR-COUNT)
               END-READ
           END-PERFORM.
           CLOSE INPUT-FILE.

       CALCULATE-FREQUENCIES.
           PERFORM VARYING CHAR-INDEX FROM 1 BY 1
               UNTIL CHAR-INDEX > CHAR-COUNT
               COMPUTE CHAR-INDEX = FUNCTION ORD(INPUT-CHAR(CHAR-INDEX))
               ADD 1 TO CHAR-FREQUENCY(CHAR-INDEX)
               ADD 1 TO TOTAL-FREQUENCY
           END-PERFORM.

       CALCULATE-PROBABILITIES.
           PERFORM VARYING CHAR-INDEX FROM 0 BY 1
               UNTIL CHAR-INDEX > 255
               IF CHAR-FREQUENCY(CHAR-INDEX) > 0
                   COMPUTE CHAR-PROBABILITY(CHAR-INDEX) =
                       CHAR-FREQUENCY(CHAR-INDEX) / TOTAL-FREQUENCY
               END-IF
           END-PERFORM.

       ENCODE-TEXT.
           MOVE 0.0 TO LOWER-BOUND, UPPER-BOUND, RANGE.
           MOVE 1.0 TO RANGE.
           PERFORM VARYING CHAR-INDEX FROM 1 BY 1
               UNTIL CHAR-INDEX > CHAR-COUNT
               PERFORM ENCODE-CHARACTER
           END-PERFORM.

       ENCODE-CHARACTER.
           COMPUTE CHAR-INDEX = FUNCTION ORD(INPUT-CHAR(CHAR-INDEX))
           COMPUTE LOWER-BOUND = LOWER-BOUND +
               (RANGE * FUNCTION LEADING-ZEROES(CHAR-CUMULATIVE(CHAR-INDEX)))
           COMPUTE UPPER-BOUND = LOWER-BOUND +
               (RANGE * CHAR-PROBABILITY(CHAR-INDEX))
           COMPUTE RANGE = UPPER-BOUND - LOWER-BOUND.

       WRITE-OUTPUT.
           OPEN OUTPUT OUTPUT-FILE.
           MOVE LOWER-BOUND TO CODING-RESULT.
           WRITE OUTPUT-RECORD FROM CODING-RESULT.
           CLOSE OUTPUT-FILE.

       DISPLAY-RESULTS.
           DISPLAY "TOTAL CHARACTERS: " TOTAL-CHARS.
           DISPLAY "TOTAL FREQUENCY: " TOTAL-FREQUENCY.
           PERFORM VARYING CHAR-INDEX FROM 0 BY 1
               UNTIL CHAR-INDEX > 255
               IF CHAR-FREQUENCY(CHAR-INDEX) > 0
                   DISPLAY "Character " CHAR-INDEX 
                           " frequency: " CHAR-FREQUENCY(CHAR-INDEX)
               END-IF
           END-PERFORM.
```

## Explanation

This COBOL program demonstrates the arithmetic coding algorithm with the following key components:

### Key Features:
1. **Frequency Table**: Stores character frequencies (256 entries for ASCII characters)
2. **Probability Calculation**: Computes probabilities from frequency counts
3. **Encoding Process**: Implements the core arithmetic coding algorithm
4. **Output Generation**: Produces compressed coded values

### Algorithm Steps:
1. **Initialize** frequency and probability tables
2. **Read Input Data** character by character
3. **Calculate Frequencies** for each character
4. **Compute Probabilities** from frequencies
5. **Encode Text** using arithmetic coding principles
6. **Output Result** as compressed value

### Key Variables:
- `LOWER-BOUND` and `UPPER-BOUND`: Define the current interval
- `RANGE`: Size of the current interval
- `CHAR-FREQUENCY`: Count of each character
- `CHAR-PROBABILITY`: Probability of each character

This implementation provides a foundation for understanding arithmetic coding concepts in COBOL, though real-world applications would require more sophisticated handling of floating-point precision and larger data sets.