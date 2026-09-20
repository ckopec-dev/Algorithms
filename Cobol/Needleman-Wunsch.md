# Needleman-Wunsch Algorithm in COBOL

The Needleman-Wunsch algorithm is a dynamic programming approach for global sequence alignment. Here's an implementation in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NEEDLEMAN-WUNSCH.
       AUTHOR. Sequence Alignment Algorithm.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQUENCE-FILE ASSIGN TO "sequences.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  SEQUENCE-FILE.
       01  SEQUENCE-RECORD.
           05  SEQ1          PIC X(50).
           05  SEQ2          PIC X(50).

       WORKING-STORAGE SECTION.
       01  SEQ1-LENGTH      PIC 99 VALUE 0.
       01  SEQ2-LENGTH      PIC 99 VALUE 0.
       01  GAP-PENALTY      PIC 99 VALUE 2.
       01  MATCH-PENALTY    PIC 99 VALUE 0.
       01  MISMATCH-PENALTY PIC 99 VALUE 1.
       
       01  MATRIX.
           05  MATRIX-ROW OCCURS 50 TIMES.
               10  MATRIX-ELEMENT OCCURS 50 TIMES PIC 99.

       01  I                  PIC 99 VALUE 0.
       01  J                  PIC 99 VALUE 0.
       01  DIAG               PIC 99 VALUE 0.
       01  UP                 PIC 99 VALUE 0.
       01  LEFT               PIC 99 VALUE 0.
       01  MAX-VALUE          PIC 99 VALUE 0.
       01  MATCH              PIC X VALUE SPACE.
       01  MATCH-FLAG         PIC 99 VALUE 0.

       01  OUTPUT-STRING      PIC X(100).
       01  TEMP-STRING        PIC X(50).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "NEEDLEMAN-WUNSCH SEQUENCE ALIGNMENT"
           DISPLAY "===================================="

           OPEN INPUT SEQUENCE-FILE
           READ SEQUENCE-FILE
               AT END GO TO END-PROGRAM
           END-READ

           MOVE SEQ1 TO TEMP-STRING
           COMPUTE SEQ1-LENGTH = FUNCTION LENGTH(TEMP-STRING)

           MOVE SEQ2 TO TEMP-STRING
           COMPUTE SEQ2-LENGTH = FUNCTION LENGTH(TEMP-STRING)

           DISPLAY "Sequence 1: " SEQ1
           DISPLAY "Sequence 2: " SEQ2
           DISPLAY "Length 1: " SEQ1-LENGTH
           DISPLAY "Length 2: " SEQ2-LENGTH

           PERFORM INITIALIZE-MATRIX
           PERFORM FILL-MATRIX
           PERFORM TRACE-BACK
           PERFORM DISPLAY-RESULTS

           CLOSE SEQUENCE-FILE
           STOP RUN.

       INITIALIZE-MATRIX.
           DISPLAY "Initializing Matrix..."
           
           *> Initialize first row with gap penalties
           MOVE 0 TO I
           PERFORM VARYING I FROM 0 BY 1 UNTIL I > SEQ2-LENGTH
               MOVE I * GAP-PENALTY TO MATRIX-ELEMENT(0,I)
           END-PERFORM

           *> Initialize first column with gap penalties
           MOVE 0 TO I
           PERFORM VARYING I FROM 0 BY 1 UNTIL I > SEQ1-LENGTH
               MOVE I * GAP-PENALTY TO MATRIX-ELEMENT(I,0)
           END-PERFORM.

       FILL-MATRIX.
           DISPLAY "Filling Matrix..."
           
           *> Fill the matrix using dynamic programming
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SEQ1-LENGTH
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > SEQ2-LENGTH
                   *> Check if characters match
                   IF SEQ1(I:1) = SEQ2(J:1)
                       MOVE 0 TO MATCH-PENALTY
                       MOVE 1 TO MATCH-FLAG
                   ELSE
                       MOVE 1 TO MISMATCH-PENALTY
                       MOVE 0 TO MATCH-FLAG
                   END-IF

                   *> Calculate values from three possible directions
                   COMPUTE DIAG = MATRIX-ELEMENT(I-1,J-1) + MATCH-PENALTY
                   COMPUTE UP = MATRIX-ELEMENT(I-1,J) + GAP-PENALTY
                   COMPUTE LEFT = MATRIX-ELEMENT(I,J-1) + GAP-PENALTY

                   *> Find maximum value
                   COMPUTE MAX-VALUE = DIAG
                   IF UP > MAX-VALUE
                       MOVE UP TO MAX-VALUE
                   END-IF
                   IF LEFT > MAX-VALUE
                       MOVE LEFT TO MAX-VALUE
                   END-IF

                   MOVE MAX-VALUE TO MATRIX-ELEMENT(I,J)
               END-PERFORM
           END-PERFORM.

       TRACE-BACK.
           DISPLAY "Tracing back..."
           
           *> Not implemented in this example - would trace back through matrix
           *> to find optimal alignment path
           MOVE 0 TO I
           MOVE 0 TO J.

       DISPLAY-RESULTS.
           DISPLAY "Final Alignment Score: " MATRIX-ELEMENT(SEQ1-LENGTH,SEQ2-LENGTH)
           DISPLAY "Matrix:"
           
           PERFORM VARYING I FROM 0 BY 1 UNTIL I > SEQ1-LENGTH
               MOVE 0 TO J
               PERFORM VARYING J FROM 0 BY 1 UNTIL J > SEQ2-LENGTH
                   IF I = 0 AND J = 0
                       DISPLAY "   0 "
                   ELSE IF I = 0 OR J = 0
                       DISPLAY MATRIX-ELEMENT(I,J) " "
                   ELSE
                       DISPLAY MATRIX-ELEMENT(I,J) " "
                   END-IF
               END-PERFORM
               DISPLAY SPACE
           END-PERFORM.

       END-PROGRAM.
```

## Example Usage

To use this program, you would need a data file `sequences.dat` containing two sequences:

```
ACGTACGT
TGCATGCA
```

## Key Features of This Implementation

1. **Dynamic Programming Matrix**: Creates a 2D matrix to store alignment scores
2. **Gap Penalties**: Implements gap penalty scoring system
3. **Match/Mismatch Scoring**: Differentiates between matching and non-matching characters
4. **Matrix Initialization**: Properly initializes the first row and column
5. **Score Calculation**: Computes maximum score from three possible directions (diagonal, up, left)

## Algorithm Steps

1. Initialize matrix with gap penalties
2. Fill matrix using recurrence relation:
   ```
   S(i,j) = max{S(i-1,j-1)+match_score, S(i-1,j)+gap_penalty, S(i,j-1)+gap_penalty}
   ```
3. Trace back through matrix to find optimal alignment

This implementation demonstrates the core principles of the Needleman-Wunsch algorithm in COBOL syntax and structure.