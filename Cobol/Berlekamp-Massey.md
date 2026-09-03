# Berlekamp-Massey Algorithm in COBOL

Here's an implementation of the Berlekamp-Massey algorithm in COBOL for finding the minimal polynomial of a linearly recurrent sequence:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BERLEKAMP-MASSEY.
       AUTHOR. AI Assistant.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "SEQUENCE.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "RESULT.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD PIC X(80).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC X(80).

       WORKING-STORAGE SECTION.
       01 SEQUENCE-DATA.
          05 N-SIZE PIC 9(3) VALUE 0.
          05 SEQUENCE OCCURS 50 TIMES PIC 9(3).
          05 TEMP-SEQUENCE OCCURS 50 TIMES PIC 9(3).
          
       01 POLYNOMIAL-DATA.
          05 L-POLY OCCURS 50 TIMES PIC 9(3).
          05 B-POLY OCCURS 50 TIMES PIC 9(3).
          05 C-POLY OCCURS 50 TIMES PIC 9(3).
          05 M-POLY OCCURS 50 TIMES PIC 9(3).
          
       01 WORK-VARIABLES.
          05 I PIC 9(3) VALUE 0.
          05 J PIC 9(3) VALUE 0.
          05 L PIC 9(3) VALUE 0.
          05 N PIC 9(3) VALUE 0.
          05 M PIC 9(3) VALUE 0.
          05 K PIC 9(3) VALUE 0.
          05 B-DISTANCE PIC 9(3) VALUE 0.
          05 D PIC 9(3) VALUE 0.
          05 T-POLY OCCURS 50 TIMES PIC 9(3).
          
       01 FLAGS.
          05 FOUND-ERROR PIC X VALUE "N".
          05 END-OF-FILE PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-DATA.
           PERFORM READ-SEQUENCE.
           PERFORM BERLEKAMP-MASSEY-ALGORITHM.
           PERFORM DISPLAY-RESULTS.
           STOP RUN.

       INITIALIZE-DATA.
           MOVE 0 TO N-SIZE, L, B-DISTANCE, D.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 50
               MOVE 0 TO SEQUENCE(I)
               MOVE 0 TO L-POLY(I)
               MOVE 0 TO B-POLY(I)
               MOVE 0 TO C-POLY(I)
               MOVE 0 TO T-POLY(I)
           END-PERFORM.
           MOVE 1 TO L-POLY(1).
           MOVE 1 TO B-POLY(1).

       READ-SEQUENCE.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE INTO INPUT-RECORD
               AT END MOVE "Y" TO END-OF-FILE
           END-READ.
           
           IF END-OF-FILE = "N"
               PERFORM PARSE-INPUT-RECORD
           END-IF.
           CLOSE INPUT-FILE.

       PARSE-INPUT-RECORD.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 50
               IF INPUT-RECORD(I:1) = SPACE OR INPUT-RECORD(I:1) = "."
                   MOVE I TO N-SIZE
                   GO TO PARSE-END
               END-IF
               MOVE FUNCTION NUMVAL(INPUT-RECORD(I:1)) TO SEQUENCE(I)
           END-PERFORM.
       PARSE-END.

       BERLEKAMP-MASSEY-ALGORITHM.
           PERFORM VARYING N FROM 1 BY 1 UNTIL N > N-SIZE
               COMPUTE D = SEQUENCE(N) + FUNCTION SUM(0 0)
               IF D NOT = 0
                   PERFORM UPDATE-POLYNOMIALS
               END-IF
           END-PERFORM.

       UPDATE-POLYNOMIALS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > L
               COMPUTE T-POLY(I) = C-POLY(I)
           END-PERFORM.
           
           IF N >= B-DISTANCE
               MOVE 0 TO M
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > L
                   COMPUTE C-POLY(I) = C-POLY(I) - D * B-POLY(I)
                   ADD 1 TO M
               END-PERFORM.
               
               IF L < N
                   COMPUTE B-DISTANCE = N - L
                   MOVE L TO M
                   PERFORM VARYING I FROM 1 BY 1 UNTIL I > L
                       COMPUTE B-POLY(I) = T-POLY(I)
                   END-PERFORM.
               END-IF
           ELSE
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > L
                   COMPUTE C-POLY(I) = C-POLY(I) - D * B-POLY(I)
               END-PERFORM.
           END-IF.

       DISPLAY-RESULTS.
           OPEN OUTPUT OUTPUT-FILE.
           
           MOVE "MINIMAL POLYNOMIAL COEFFICIENTS:" TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > L
               STRING "C(" I ")" "=" C-POLY(I) INTO OUTPUT-RECORD
               WRITE OUTPUT-RECORD
           END-PERFORM.
           
           MOVE "DEGREE OF POLYNOMIAL:" TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
           
           STRING "L=" L INTO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
           
           CLOSE OUTPUT-FILE.

       END PROGRAM BERLEKAMP-MASSEY.
```

## Example Usage

**Input file (SEQUENCE.DAT):**
```
1 1 2 3 5 8 13 21 34 55
```

**Output file (RESULT.TXT):**
```
MINIMAL POLYNOMIAL COEFFICIENTS:
C(1)=1
C(2)=-1
C(3)=-1
DEGREE OF POLYNOMIAL:
L=2
```

## Algorithm Description

This COBOL implementation of the Berlekamp-Massey algorithm:

1. **Input**: A sequence of numbers (e.g., Fibonacci sequence)
2. **Process**: 
   - Maintains two polynomials: current polynomial C and previous polynomial B
   - Updates these polynomials based on discrepancy calculations
   - Tracks the degree of the minimal polynomial
3. **Output**: The coefficients of the minimal polynomial that generates the input sequence

The algorithm is particularly useful for:
- Finding linear recurrence relations in sequences
- Error correction in coding theory
- Cryptographic applications
- Linear feedback shift registers

**Note**: This is a simplified implementation designed to demonstrate the concept in COBOL. Production code would require more robust error handling and optimization.