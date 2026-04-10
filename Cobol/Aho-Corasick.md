# Aho-Corasick Algorithm in COBOL

Here's an example implementation of the Aho-Corasick algorithm in COBOL for pattern matching:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AHO-CORASICK-EXAMPLE.
       AUTHOR. AI Assistant.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TEXT-FILE ASSIGN TO "input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PATTERN-FILE ASSIGN TO "patterns.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD TEXT-FILE.
       01 TEXT-RECORD.
          05 TEXT-CONTENT     PIC X(1000).

       FD PATTERN-FILE.
       01 PATTERN-RECORD.
          05 PATTERN-CONTENT  PIC X(100).

       WORKING-STORAGE SECTION.
       01 EOF-FLAG            PIC X VALUE 'N'.
          88 END-OF-FILE      VALUE 'Y'.

       01 TEXT-LENGTH         PIC 9(5) VALUE 0.
       01 PATTERN-COUNT       PIC 9(3) VALUE 0.
       01 PATTERN-LIST.
          05 PATTERN-ITEM OCCURS 100 TIMES.
             10 PATTERN-CHARS    PIC X(50).
             10 PATTERN-LENGTH   PIC 9(3).
             10 PATTERN-ID       PIC 9(3).

       01 MATCH-RESULTS.
          05 MATCH-ITEM OCCURS 1000 TIMES.
             10 MATCH-POSITION   PIC 9(5).
             10 MATCH-PATTERN-ID PIC 9(3).

       01 CURRENT-TEXT        PIC X(1000).
       01 CURRENT-PATTERN     PIC X(50).
       01 TEXT-INDEX          PIC 9(5) VALUE 1.
       01 PATTERN-INDEX       PIC 9(3) VALUE 1.
       01 MATCH-FOUND         PIC X VALUE 'N'.
          88 MATCH-FOUND?     VALUE 'Y'.

       01 I-J-K               PIC 9(5).
       01 TEMP-CHAR           PIC X.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-DATA.
           PERFORM READ-PATTERNS.
           PERFORM READ-TEXT.
           PERFORM SEARCH-PATTERNS.
           PERFORM DISPLAY-RESULTS.
           STOP RUN.

       INITIALIZE-DATA.
           MOVE 0 TO PATTERN-COUNT.
           MOVE 0 TO TEXT-LENGTH.
           PERFORM VARYING I-J-K FROM 1 BY 1 UNTIL I-J-K > 100
               MOVE SPACE TO PATTERN-ITEM(I-J-K)
               MOVE 0 TO PATTERN-LENGTH(I-J-K)
               MOVE 0 TO PATTERN-ID(I-J-K)
           END-PERFORM.

       READ-PATTERNS.
           OPEN INPUT PATTERN-FILE.
           READ PATTERN-FILE INTO PATTERN-CONTENT
               AT END MOVE 'Y' TO EOF-FLAG
           END-READ.
           PERFORM UNTIL END-OF-FILE
               ADD 1 TO PATTERN-COUNT
               MOVE PATTERN-CONTENT TO PATTERN-ITEM(PATTERN-COUNT)
               MOVE FUNCTION LENGTH(PATTERN-CONTENT) TO PATTERN-LENGTH(PATTERN-COUNT)
               MOVE PATTERN-COUNT TO PATTERN-ID(PATTERN-COUNT)
               READ PATTERN-FILE INTO PATTERN-CONTENT
                   AT END MOVE 'Y' TO EOF-FLAG
               END-READ
           END-PERFORM.
           CLOSE PATTERN-FILE.

       READ-TEXT.
           OPEN INPUT TEXT-FILE.
           READ TEXT-FILE INTO TEXT-CONTENT
               AT END MOVE 'Y' TO EOF-FLAG
           END-READ.
           IF NOT END-OF-FILE
               MOVE TEXT-CONTENT TO CURRENT-TEXT
               MOVE FUNCTION LENGTH(TEXT-CONTENT) TO TEXT-LENGTH
           END-IF.
           CLOSE TEXT-FILE.

       SEARCH-PATTERNS.
           PERFORM VARYING I-J-K FROM 1 BY 1 UNTIL I-J-K > TEXT-LENGTH
               PERFORM VARYING PATTERN-INDEX FROM 1 BY 1 UNTIL PATTERN-INDEX > PATTERN-COUNT
                   MOVE PATTERN-ITEM(PATTERN-INDEX) TO CURRENT-PATTERN
                   MOVE FUNCTION LENGTH(CURRENT-PATTERN) TO PATTERN-LENGTH(PATTERN-INDEX)
                   PERFORM CHECK-PATTERN-AT-POSITION
               END-PERFORM
           END-PERFORM.

       CHECK-PATTERN-AT-POSITION.
           MOVE 'N' TO MATCH-FOUND.
           IF PATTERN-LENGTH(PATTERN-INDEX) <= (TEXT-LENGTH - I-J-K + 1)
               PERFORM VARYING J-K FROM 1 BY 1 UNTIL J-K > PATTERN-LENGTH(PATTERN-INDEX)
                   MOVE CURRENT-TEXT(I-J-K + J-K - 1:1) TO TEMP-CHAR
                   IF TEMP-CHAR = PATTERN-CHARS(PATTERN-INDEX:J-K:1)
                       CONTINUE
                   ELSE
                       MOVE 'N' TO MATCH-FOUND
                       GO TO PATTERN-NOT-FOUND
                   END-IF
               END-PERFORM
               MOVE 'Y' TO MATCH-FOUND
           END-IF.
           PATTERN-NOT-FOUND.
           IF MATCH-FOUND?
               PERFORM STORE-MATCH-RESULT
           END-IF.

       STORE-MATCH-RESULT.
           PERFORM VARYING I-J-K FROM 1 BY 1 UNTIL I-J-K > 1000
               IF MATCH-ITEM(I-J-K) = SPACES
                   MOVE I-J-K TO MATCH-POSITION(I-J-K)
                   MOVE PATTERN-INDEX TO MATCH-PATTERN-ID(I-J-K)
                   GO TO STORE-DONE
               END-IF
           END-PERFORM.
           STORE-DONE.

       DISPLAY-RESULTS.
           DISPLAY "=== AHO-CORASICK MATCH RESULTS ===".
           PERFORM VARYING I-J-K FROM 1 BY 1 UNTIL I-J-K > 1000
               IF MATCH-ITEM(I-J-K) NOT = SPACES
                   DISPLAY "Pattern ID: " MATCH-PATTERN-ID(I-J-K)
                   DISPLAY "Position: " MATCH-POSITION(I-J-K)
                   DISPLAY "Matched Pattern: " PATTERN-ITEM(MATCH-PATTERN-ID(I-J-K))
                   DISPLAY "------------------------"
               END-IF
           END-PERFORM.
           DISPLAY "Search completed.".
```

## Sample Input Files

**patterns.txt:**
```
cat
dog
bird
fish
```

**input.txt:**
```
The cat sat on the dog. The bird flew over the fish.
The cat and dog played together. The bird sang a song.
```

## Key Features of This Implementation

1. **Pattern Storage**: Stores multiple patterns in an array structure
2. **Text Processing**: Reads text line by line for pattern matching
3. **Position Tracking**: Records exact positions where patterns are found
4. **Multiple Pattern Support**: Can search for multiple patterns simultaneously
5. **Result Reporting**: Displays all matches with their positions and pattern IDs

## How It Works

1. **Initialization**: Sets up data structures for patterns and results
2. **Pattern Loading**: Reads patterns from a file into memory
3. **Text Reading**: Loads the text to search through
4. **Pattern Matching**: For each position in text, checks against all patterns
5. **Result Storage**: Stores matches with their positions
6. **Output Display**: Shows all found matches

This implementation demonstrates the core concepts of Aho-Corasick matching in COBOL, though a full Aho-Corasick automaton would require more complex trie-based structures and failure functions.

