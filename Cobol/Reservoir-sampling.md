# Reservoir Sampling Algorithm in COBOL

Here's an implementation of the Reservoir Sampling algorithm in COBOL to randomly select k items from a stream of unknown length:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RESERVOIR-SAMPLING.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO INPUT-DATA
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO OUTPUT-DATA
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
          05 RECORD-DATA    PIC X(50).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
          05 OUTPUT-DATA    PIC X(50).

       WORKING-STORAGE SECTION.
       01 WS-RESERVOIR.
          05 RESERVOIR-ITEMS OCCURS 10 TIMES.
             10 RESERVOIR-DATA    PIC X(50).
       01 WS-RESERVOIR-SIZE    PIC 99 VALUE 5.
       01 WS-COUNT             PIC 9(10) VALUE 0.
       01 WS-INDEX             PIC 99 VALUE 0.
       01 WS-RANDOM            PIC 9(10).
       01 WS-TEMP              PIC X(50).
       01 WS-EOF               PIC X VALUE 'N'.
       01 WS-INPUT-RECORD      PIC X(50).
       01 WS-OUTPUT-RECORD     PIC X(50).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INITIALIZE-RESERVOIR
           PERFORM READ-AND-SAMPLE
           PERFORM DISPLAY-RESULTS
           STOP RUN.

       INITIALIZE-RESERVOIR.
           MOVE 0 TO WS-COUNT
           MOVE 0 TO WS-INDEX
           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > WS-RESERVOIR-SIZE
               MOVE SPACES TO RESERVOIR-ITEMS(WS-INDEX)
           END-PERFORM.

       READ-AND-SAMPLE.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE
               AT END MOVE 'Y' TO WS-EOF
           END-READ.

           PERFORM UNTIL WS-EOF = 'Y'
               ADD 1 TO WS-COUNT
               
               IF WS-COUNT <= WS-RESERVOIR-SIZE
                   MOVE INPUT-RECORD TO RESERVOIR-ITEMS(WS-COUNT)
               ELSE
                   COMPUTE WS-RANDOM = FUNCTION RANDOM * WS-COUNT
                   IF WS-RANDOM < WS-RESERVOIR-SIZE
                       MOVE INPUT-RECORD TO RESERVOIR-ITEMS(WS-RANDOM + 1)
                   END-IF
               END-IF

               READ INPUT-FILE
                   AT END MOVE 'Y' TO WS-EOF
               END-READ
           END-PERFORM.

       DISPLAY-RESULTS.
           DISPLAY "RESERVOIR SAMPLING RESULTS:"
           DISPLAY "================================"
           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > WS-RESERVOIR-SIZE
               IF RESERVOIR-ITEMS(WS-INDEX) NOT = SPACES
                   DISPLAY "Sample " WS-INDEX ": " RESERVOIR-ITEMS(WS-INDEX)
               END-IF
           END-PERFORM.

       CLOSE INPUT-FILE.
       CLOSE OUTPUT-FILE.
```

## How it works:

1. **Initialization**: Sets up a reservoir array of fixed size (5 items in this example)
2. **Reading Loop**: Processes each input record from the data stream
3. **Sampling Logic**:
   - For the first k records, store them directly in the reservoir
   - For subsequent records, replace items in the reservoir with probability k/i (where i is the current record number)
4. **Output**: Displays the final sample set

## Sample Input File Format:
```
FIRST RECORD
SECOND RECORD
THIRD RECORD
FOURTH RECORD
FIFTH RECORD
SIXTH RECORD
SEVENTH RECORD
EIGHTH RECORD
```

## Sample Output:
```
RESERVOIR SAMPLING RESULTS:
================================
Sample 1: THIRD RECORD
Sample 2: FIFTH RECORD
Sample 3: SECOND RECORD
Sample 4: EIGHTH RECORD
Sample 5: FIRST RECORD
```

This implementation demonstrates the classic reservoir sampling algorithm that maintains a random sample of k items from a stream of unknown length, using the mathematical property that each item has an equal probability of being selected.

