# Cocke-Younger-Kasami (CYK) Parsing Algorithm in COBOL

The following is a COBOL implementation of the CYK parsing algorithm for context-free grammars:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CYK-PARSER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GRAMMAR-FILE ASSIGN TO "GRAMMAR.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SENTENCE-FILE ASSIGN TO "SENTENCE.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD GRAMMAR-FILE.
       01 GRAMMAR-RECORD.
           05 LEFT-SYMBOL    PIC X(1).
           05 RIGHT-SYMBOLS  PIC X(10).

       FD SENTENCE-FILE.
       01 SENTENCE-RECORD.
           05 SENTENCE-TEXT  PIC X(50).

       WORKING-STORAGE SECTION.
       01 GRAMMAR-TABLE.
           05 GRAMMAR-ROW    OCCURS 20 TIMES.
               10 LEFT-SYMBOL-WS   PIC X(1).
               10 RIGHT-SYMBOLS-WS PIC X(10).
       01 GRAMMAR-COUNT     PIC 99 VALUE 0.

       01 PARSE-TABLE.
           05 PARSE-CELL    OCCURS 20 TIMES DEPENDING ON SENTENCE-LENGTH.
               10 NONTERMINALS  OCCURS 10 TIMES.
                   15 NONTERM-NAME PIC X(1).
                   15 IS-POSSIBLE  PIC 9 VALUE 0.

       01 SENTENCE-LENGTH   PIC 99 VALUE 0.
       01 SENTENCE-ARRAY.
           05 CHAR-ARRAY     OCCURS 20 TIMES PIC X(1).

       01 WORK-VARIABLES.
           05 I                PIC 99 VALUE 0.
           05 J                PIC 99 VALUE 0.
           05 K                PIC 99 VALUE 0.
           05 L                PIC 99 VALUE 0.
           05 M                PIC 99 VALUE 0.
           05 N                PIC 99 VALUE 0.
           05 MATCH-FLAG       PIC 9 VALUE 0.
           05 FOUND-FLAG       PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM INITIALIZE-DATA
           PERFORM READ-GRAMMAR
           PERFORM READ-SENTENCE
           PERFORM CYK-ALGORITHM
           PERFORM DISPLAY-RESULTS
           STOP RUN.

       INITIALIZE-DATA.
           MOVE 0 TO GRAMMAR-COUNT.
           MOVE 0 TO SENTENCE-LENGTH.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 20
               MOVE SPACES TO CHAR-ARRAY(I)
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 10
                   MOVE 0 TO PARSE-CELL(I)(J)
               END-PERFORM
           END-PERFORM.

       READ-GRAMMAR.
           OPEN INPUT GRAMMAR-FILE
           READ GRAMMAR-FILE AT END GO TO GRAMMAR-END
           PERFORM PROCESS-GRAMMAR-RECORD
           PERFORM READ-GRAMMAR
           GRAMMAR-END.
           CLOSE GRAMMAR-FILE.

       PROCESS-GRAMMAR-RECORD.
           ADD 1 TO GRAMMAR-COUNT
           MOVE LEFT-SYMBOL TO LEFT-SYMBOL-WS(GRAMMAR-COUNT)
           MOVE RIGHT-SYMBOLS TO RIGHT-SYMBOLS-WS(GRAMMAR-COUNT).

       READ-SENTENCE.
           OPEN INPUT SENTENCE-FILE
           READ SENTENCE-FILE AT END GO TO SENTENCE-END
           PERFORM PROCESS-SENTENCE-RECORD
           SENTENCE-END.
           CLOSE SENTENCE-FILE.

       PROCESS-SENTENCE-RECORD.
           MOVE 0 TO SENTENCE-LENGTH
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 50
               IF SENTENCE-TEXT(I) NOT = SPACES
                   ADD 1 TO SENTENCE-LENGTH
                   MOVE SENTENCE-TEXT(I) TO CHAR-ARRAY(SENTENCE-LENGTH)
               ELSE
                   GO TO PROCESS-SENTENCE-END
               END-IF
           END-PERFORM.
       PROCESS-SENTENCE-END.

       CYK-ALGORITHM.
           PERFORM INITIALIZE-TABLE
           PERFORM FILL-DIAGONAL
           PERFORM FILL-UPPER-TRIANGLE.

       INITIALIZE-TABLE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SENTENCE-LENGTH
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 10
                   MOVE 0 TO PARSE-CELL(I)(J)
               END-PERFORM
           END-PERFORM.

       FILL-DIAGONAL.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SENTENCE-LENGTH
               PERFORM CHECK-GRAMMAR-RULES FOR-CHAR-I(I)
           END-PERFORM.

       CHECK-GRAMMAR-RULES FOR-CHAR-I.
           MOVE 0 TO FOUND-FLAG
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRAMMAR-COUNT
               IF RIGHT-SYMBOLS-WS(J) = CHAR-ARRAY(I)
                   ADD 1 TO PARSE-CELL(I)(J)
                   MOVE 1 TO PARSE-CELL(I)(J)
                   MOVE LEFT-SYMBOL-WS(J) TO NONTERM-NAME(J)
               END-IF
           END-PERFORM.

       FILL-UPPER-TRIANGLE.
           PERFORM VARYING L FROM 2 BY 1 UNTIL L > SENTENCE-LENGTH
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > (SENTENCE-LENGTH - L + 1)
                   COMPUTE J = I + L - 1
                   PERFORM PARTITION-CELL(I,J)
               END-PERFORM
           END-PERFORM.

       PARTITION-CELL.
           PERFORM VARYING K FROM I BY 1 UNTIL K >= J
               PERFORM COMBINE-CELLS(I,K,J)
           END-PERFORM.

       COMBINE-CELLS.
           PERFORM VARYING M FROM 1 BY 1 UNTIL M > GRAMMAR-COUNT
               IF RIGHT-SYMBOLS-WS(M) = CHAR-ARRAY(K)
                   PERFORM VARYING N FROM 1 BY 1 UNTIL N > 10
                       IF PARSE-CELL(I)(N) = 1 AND PARSE-CELL(K+1)(N) = 1
                           MOVE 1 TO PARSE-CELL(I)(M)
                           MOVE LEFT-SYMBOL-WS(M) TO NONTERM-NAME(M)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM.

       DISPLAY-RESULTS.
           DISPLAY "CYK PARSING RESULTS:"
           IF PARSE-CELL(1)(1) = 1
               DISPLAY "Sentence is in the language"
           ELSE
               DISPLAY "Sentence is NOT in the language"
           END-IF.

       END PROGRAM CYK-PARSER.
```

## Example Grammar File (GRAMMAR.DAT)
```
SAB
ABD
BCD
Aa
Bb
Cc
```

## Example Sentence File (SENTENCE.DAT)
```
abc
```

## Key Features of This Implementation

1. **Grammar Representation**: Stores grammar rules in a table format
2. **Parse Table**: Uses a 2D array to store parsing results
3. **Dynamic Programming**: Implements the CYK algorithm with bottom-up approach
4. **Diagonal Filling**: First fills diagonal elements with terminal symbols
5. **Triangular Filling**: Then fills upper triangle using grammar rules

## Algorithm Steps

1. **Initialization**: Create parse table and initialize with grammar rules
2. **Diagonal Filling**: Fill first diagonal with terminals from input sentence
3. **Upper Triangle Filling**: For each substring, check all possible partitions
4. **Rule Application**: Apply grammar rules to combine non-terminals
5. **Result**: Check if start symbol appears in final cell

This implementation demonstrates the core CYK parsing algorithm using COBOL's structured programming capabilities and data handling features.