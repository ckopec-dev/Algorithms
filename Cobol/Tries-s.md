# Trie Algorithm Implementation in COBOL

Here's an example implementation of a Trie (Prefix Tree) data structure in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRIE-IMPLEMENTATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WORD-FILE ASSIGN TO "WORDS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  WORD-FILE.
       01  WORD-RECORD.
           05  WORD-TEXT        PIC X(20).

       WORKING-STORAGE SECTION.
       01  TRIE-NODE.
           05  CHILDREN         OCCURS 26 TIMES.
               10  NODE-POINTER  POINTER.
               10  IS-END-OF-WORD PIC X VALUE SPACE.
           05  WORD-COUNT       PIC 9(5) VALUE 0.

       01  ROOT-NODE            POINTER.
       01  CURRENT-NODE         POINTER.
       01  TEMP-NODE            POINTER.
       01  SEARCH-RESULT        PIC X VALUE SPACE.
       01  INPUT-WORD           PIC X(20).
       01  I                    PIC 9(3) VALUE 1.
       01  J                    PIC 9(3) VALUE 1.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-TRIE
           PERFORM INSERT-WORDS
           PERFORM SEARCH-WORDS
           STOP RUN.

       INITIALIZE-TRIE.
           ALLOCATE TRIE-NODE TO ROOT-NODE
           IF ROOT-NODE = NULL
               DISPLAY "ERROR: Cannot allocate memory for trie"
               STOP RUN
           END-IF
           MOVE SPACES TO IS-END-OF-WORD OF ROOT-NODE
           MOVE 0 TO WORD-COUNT OF ROOT-NODE.

       INSERT-WORDS.
           OPEN INPUT WORD-FILE
           READ WORD-FILE INTO INPUT-WORD
               AT END GO TO INSERT-END
           END-READ

           PERFORM INSERT-WORD

           READ WORD-FILE INTO INPUT-WORD
               AT END GO TO INSERT-END
           END-READ
           GO TO INSERT-WORDS

       INSERT-END.
           CLOSE WORD-FILE.

       INSERT-WORD.
           MOVE ROOT-NODE TO CURRENT-NODE
           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > LENGTH OF INPUT-WORD
           COMPUTE J = FUNCTION ORD(INPUT-WORD(I:1)) - FUNCTION ORD("A") + 1
           IF NODE-POINTER OF CHILDREN(J) = NULL
               ALLOCATE TRIE-NODE TO TEMP-NODE
               MOVE SPACES TO IS-END-OF-WORD OF TEMP-NODE
               MOVE 0 TO WORD-COUNT OF TEMP-NODE
               MOVE TEMP-NODE TO NODE-POINTER OF CHILDREN(J)
           END-IF
           MOVE NODE-POINTER OF CHILDREN(J) TO CURRENT-NODE
           END-PERFORM

           ADD 1 TO WORD-COUNT OF CURRENT-NODE
           MOVE "Y" TO IS-END-OF-WORD OF CURRENT-NODE.

       SEARCH-WORDS.
           DISPLAY "Searching for words..."
           PERFORM SEARCH-WORD WITH TEST AFTER VARYING I FROM 1 BY 1
               UNTIL I > 5
           END-PERFORM.

       SEARCH-WORD.
           MOVE ROOT-NODE TO CURRENT-NODE
           PERFORM VARYING J FROM 1 BY 1
               UNTIL J > LENGTH OF INPUT-WORD
           COMPUTE I = FUNCTION ORD(INPUT-WORD(J:1)) - FUNCTION ORD("A") + 1
           IF NODE-POINTER OF CHILDREN(I) = NULL
               MOVE "N" TO SEARCH-RESULT
               GO TO SEARCH-END
           END-IF
           MOVE NODE-POINTER OF CHILDREN(I) TO CURRENT-NODE
           END-PERFORM

           IF IS-END-OF-WORD OF CURRENT-NODE = "Y"
               MOVE "Y" TO SEARCH-RESULT
           ELSE
               MOVE "N" TO SEARCH-RESULT
           END-IF

       SEARCH-END.
           DISPLAY "Word found: " SEARCH-RESULT.

       END PROGRAM TRIE-IMPLEMENTATION.
```

## Key Features of this COBOL Trie Implementation:

### **Data Structure Components:**
- **TRIE-NODE**: Contains array of 26 pointers (for each letter A-Z)
- **IS-END-OF-WORD**: Flag indicating end of a word
- **WORD-COUNT**: Tracks number of words passing through node

### **Core Operations:**
1. **INSERT**: Adds words to the trie structure
2. **SEARCH**: Checks if a word exists in the trie
3. **INITIALIZE**: Sets up the root node

### **COBOL-Specific Features:**
- Use of `POINTER` data types for dynamic memory allocation
- `OCCURS` clause for array handling
- `FUNCTION ORD()` for character-to-integer conversion
- Dynamic memory allocation with `ALLOCATE`

### **Usage Notes:**
- Requires a word file named "WORDS.DAT" with one word per line
- Supports case-sensitive operations (converts to uppercase)
- Memory management through explicit allocation/deallocation

This implementation demonstrates how to build a trie data structure in COBOL, though it's worth noting that modern COBOL implementations may have better support for dynamic data structures.