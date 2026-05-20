# Smith-Waterman Algorithm in COBOL

The Smith-Waterman algorithm is a dynamic programming approach for local sequence alignment. Below is a COBOL implementation:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMITH-WATERMAN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQUENCE-FILE ASSIGN TO "sequences.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  SEQUENCE-FILE.
       01  SEQUENCE-RECORD.
           05  SEQ1-CHARS    PIC X(50).
           05  SEQ2-CHARS    PIC X(50).

       WORKING-STORAGE SECTION.
       01  SEQ1            PIC X(50).
       01  SEQ2            PIC X(50).
       01  LEN1            PIC 9(3) VALUE 0.
       01  LEN2            PIC 9(3) VALUE 0.
       01  MATCH-PENALTY   PIC 9(3) VALUE 2.
       01  MISMATCH-PENALTY PIC 9(3) VALUE 3.
       01  GAP-PENALTY     PIC 9(3) VALUE 1.
       01  MAX-SCORE       PIC 9(4) VALUE 0.
       01  MAX-ROW         PIC 9(3) VALUE 0.
       01  MAX-COL         PIC 9(3) VALUE 0.
       01  I               PIC 9(3) VALUE 0.
       01  J               PIC 9(3) VALUE 0.
       01  DIAG-SCORE      PIC 9(4) VALUE 0.
       01  UP-SCORE        PIC 9(4) VALUE 0.
       01  LEFT-SCORE      PIC 9(4) VALUE 0.
       01  MAX-LOCAL       PIC 9(4) VALUE 0.
       01  SCORE           PIC 9(4) VALUE 0.
       01  CHAR1           PIC X.
       01  CHAR2           PIC X.
       01  TEMP            PIC 9(4) VALUE 0.
       01  RESULT-STRING   PIC X(100).
       01  END-OF-FILE     PIC X VALUE 'N'.

       01  SCORE-MATRIX.
           05  SCORE-ROW OCCURS 50 TIMES.
               10  SCORE-COL OCCURS 50 TIMES PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           OPEN INPUT SEQUENCE-FILE
           READ SEQUENCE-FILE
               AT END MOVE 'Y' TO END-OF-FILE
           END-READ

           IF END-OF-FILE = 'N'
               MOVE SEQ1-CHARS TO SEQ1
               MOVE SEQ2-CHARS TO SEQ2
               PERFORM INITIALIZE-MATRIX
               PERFORM FILL-MATRIX
               PERFORM TRACEBACK
               PERFORM DISPLAY-RESULTS
           END-IF

           CLOSE SEQUENCE-FILE
           STOP RUN.

       INITIALIZE-MATRIX.
           MOVE FUNCTION LENGTH(SEQ1) TO LEN1
           MOVE FUNCTION LENGTH(SEQ2) TO LEN2

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN1
               MOVE 0 TO SCORE-ROW(I)
           END-PERFORM

           PERFORM VARYING J FROM 1 BY 1 UNTIL J > LEN2
               MOVE 0 TO SCORE-COL(J)
           END-PERFORM.

       FILL-MATRIX.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN1
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > LEN2
                   MOVE SEQ1(I:1) TO CHAR1
                   MOVE SEQ2(J:1) TO CHAR2

                   IF CHAR1 = CHAR2
                       COMPUTE DIAG-SCORE = SCORE-ROW(I-1)(J-1) + MATCH-PENALTY
                   ELSE
                       COMPUTE DIAG-SCORE = SCORE-ROW(I-1)(J-1) - MISMATCH-PENALTY
                   END-IF

                   COMPUTE UP-SCORE = SCORE-ROW(I-1)(J) - GAP-PENALTY
                   COMPUTE LEFT-SCORE = SCORE-ROW(I)(J-1) - GAP-PENALTY

                   COMPUTE MAX-LOCAL = DIAG-SCORE
                   IF UP-SCORE > MAX-LOCAL
                       COMPUTE MAX-LOCAL = UP-SCORE
                   END-IF
                   IF LEFT-SCORE > MAX-LOCAL
                       COMPUTE MAX-LOCAL = LEFT-SCORE
                   END-IF

                   IF MAX-LOCAL < 0
                       COMPUTE SCORE = 0
                   ELSE
                       COMPUTE SCORE = MAX-LOCAL
                   END-IF

                   MOVE SCORE TO SCORE-ROW(I)(J)

                   IF SCORE > MAX-SCORE
                       COMPUTE MAX-SCORE = SCORE
                       MOVE I TO MAX-ROW
                       MOVE J TO MAX-COL
                   END-IF
               END-PERFORM
           END-PERFORM.

       TRACEBACK.
           MOVE MAX-SCORE TO TEMP
           MOVE MAX-ROW TO I
           MOVE MAX-COL TO J
           MOVE SPACE TO RESULT-STRING
           MOVE 100 TO LEN1

           PERFORM UNTIL TEMP = 0 OR I = 0 OR J = 0
               MOVE SEQ1(I:1) TO CHAR1
               MOVE SEQ2(J:1) TO CHAR2

               IF CHAR1 = CHAR2
                   COMPUTE DIAG-SCORE = SCORE-ROW(I-1)(J-1) + MATCH-PENALTY
               ELSE
                   COMPUTE DIAG-SCORE = SCORE-ROW(I-1)(J-1) - MISMATCH-PENALTY
               END-IF

               COMPUTE UP-SCORE = SCORE-ROW(I-1)(J) - GAP-PENALTY
               COMPUTE LEFT-SCORE = SCORE-ROW(I)(J-1) - GAP-PENALTY

               IF DIAG-SCORE = TEMP
                   SUBTRACT 1 FROM I
                   SUBTRACT 1 FROM J
                   COMPUTE TEMP = SCORE-ROW(I)(J)
               ELSE IF UP-SCORE = TEMP
                   SUBTRACT 1 FROM I
                   COMPUTE TEMP = SCORE-ROW(I)(J)
               ELSE IF LEFT-SCORE = TEMP
                   SUBTRACT 1 FROM J
                   COMPUTE TEMP = SCORE-ROW(I)(J)
               ELSE
                   COMPUTE TEMP = 0
               END-IF
           END-PERFORM.

       DISPLAY-RESULTS.
           DISPLAY "Smith-Waterman Local Alignment"
           DISPLAY "==============================="
           DISPLAY "Sequence 1: " SEQ1
           DISPLAY "Sequence 2: " SEQ2
           DISPLAY "Maximum Score: " MAX-SCORE
           DISPLAY "Start Position (Row,Col): " MAX-ROW "," MAX-COL
           DISPLAY "Alignment Result: " RESULT-STRING.
```

## Example Input File (sequences.dat):
```
ACGTACGT
TACGTACG
```

## Key Features of this Implementation:

1. **Matrix Initialization**: Creates a 50x50 score matrix
2. **Scoring System**: 
   - Match: +2 points
   - Mismatch: -3 points  
   - Gap: -1 point
3. **Local Alignment**: Uses Smith-Waterman's approach with zero scores
4. **Traceback**: Finds optimal local alignment path
5. **Memory Management**: Uses working storage for matrix and variables

## Algorithm Steps:

1. Initialize scoring matrix with zeros
2. Fill matrix using recurrence relation:
   ```
   S(i,j) = max(0, S(i-1,j-1) + match_score, S(i-1,j) - gap_penalty, S(i,j-1) - gap_penalty)
   ```
3. Track maximum score and position
4. Traceback from maximum position to find alignment

This implementation demonstrates the core principles of the Smith-Waterman algorithm in traditional COBOL syntax.

