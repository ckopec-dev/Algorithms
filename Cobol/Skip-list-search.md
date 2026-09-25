# Skip List Search Algorithm in COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SKIP-LIST-SEARCH.
       AUTHOR. COBOL IMPLEMENTATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  SKIP-LIST-STRUCTURE.
           05  MAX-LEVEL           PIC 9(2) VALUE 16.
           05  CURRENT-LEVEL       PIC 9(2) VALUE 0.
           05  HEAD-POINTER        POINTER.
           05  NODE-TABLE.
               10  NODE-RECORD REDEFINES NODE-TABLE.
                   15  NODE-KEY        PIC 9(10).
                   15  NODE-POINTERS   OCCURS 16 TIMES
                       INDEXED BY PTR-INDEX.
                       20  POINTER-VALUE POINTER.

       01  SEARCH-KEY              PIC 9(10).
       01  CURRENT-NODE            POINTER.
       01  FOUND-FLAG              PIC X VALUE 'N'.
           88  ITEM-FOUND          VALUE 'Y'.
           88  ITEM-NOT-FOUND      VALUE 'N'.
       01  TEMP-POINTER            POINTER.
       01  CURRENT-LEVEL-INDEX     PIC 9(2) VALUE 16.
       01  SEARCH-RESULT           PIC X(20).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "SKIP LIST SEARCH DEMONSTRATION"
           DISPLAY "==========================="
           
           PERFORM INITIALIZE-SKIP-LIST
           PERFORM INSERT-TEST-DATA
           PERFORM SEARCH-TESTS
           
           STOP RUN.

       INITIALIZE-SKIP-LIST.
           MOVE 16 TO CURRENT-LEVEL
           MOVE ZERO TO HEAD-POINTER
           DISPLAY "Skip List Initialized"
           .
       
       INSERT-TEST-DATA.
           DISPLAY "Inserting test data..."
           PERFORM INSERT-NODE WITH TEST AFTER VARYING I FROM 1 BY 1 
               UNTIL I > 10
               VARYING KEY-VALUE FROM 10 BY 10
               USING KEY-VALUE
           .
       
       INSERT-NODE.
           DISPLAY "Inserting key: " KEY-VALUE
           .
       
       SEARCH-TESTS.
           DISPLAY "Performing search tests..."
           
           MOVE 30 TO SEARCH-KEY
           PERFORM SEARCH-ALGORITHM
           IF ITEM-FOUND
               DISPLAY "Key " SEARCH-KEY " found in skip list"
           ELSE
               DISPLAY "Key " SEARCH-KEY " NOT found in skip list"
           END-IF
           
           MOVE 25 TO SEARCH-KEY
           PERFORM SEARCH-ALGORITHM
           IF ITEM-FOUND
               DISPLAY "Key " SEARCH-KEY " found in skip list"
           ELSE
               DISPLAY "Key " SEARCH-KEY " NOT found in skip list"
           END-IF
           .
       
       SEARCH-ALGORITHM.
           SET CURRENT-NODE TO HEAD-POINTER
           MOVE 'N' TO FOUND-FLAG
           MOVE 16 TO CURRENT-LEVEL-INDEX
           
           PERFORM UNTIL CURRENT-LEVEL-INDEX < 1
               IF CURRENT-NODE NOT = ZERO
                   IF NODE-KEY(CURRENT-NODE) > SEARCH-KEY
                       SUBTRACT 1 FROM CURRENT-LEVEL-INDEX
                   ELSE IF NODE-KEY(CURRENT-NODE) = SEARCH-KEY
                       MOVE 'Y' TO FOUND-FLAG
                       EXIT PERFORM
                   ELSE
                       SET TEMP-POINTER TO POINTER-VALUE(CURRENT-NODE, 
                           CURRENT-LEVEL-INDEX)
                       SET CURRENT-NODE TO TEMP-POINTER
                   END-IF
               ELSE
                   SUBTRACT 1 FROM CURRENT-LEVEL-INDEX
               END-IF
           END-PERFORM
           .
       
       SEARCH-SUB.
           *> This is a more detailed search implementation
           01  CURRENT-NODE-POINTER    POINTER.
           01  NEXT-NODE-POINTER       POINTER.
           01  LEVEL                   PIC 9(2) VALUE 16.
           01  COMPARISON              PIC 9(10).
           
           MOVE HEAD-POINTER TO CURRENT-NODE-POINTER
           
           PERFORM UNTIL LEVEL < 1 OR CURRENT-NODE-POINTER = ZERO
               *> Traverse down the levels
               SET NEXT-NODE-POINTER TO POINTER-VALUE(CURRENT-NODE-POINTER, 
                   LEVEL)
               
               IF NEXT-NODE-POINTER NOT = ZERO
                   COMPUTE COMPARISON = NODE-KEY(NEXT-NODE-POINTER) - SEARCH-KEY
                   IF COMPARISON < 0
                       *> Move forward
                       SET CURRENT-NODE-POINTER TO NEXT-NODE-POINTER
                   ELSE IF COMPARISON = 0
                       *> Found the key
                       MOVE 'Y' TO FOUND-FLAG
                       EXIT PERFORM
                   ELSE
                       *> Go to lower level
                       SUBTRACT 1 FROM LEVEL
                   END-IF
               ELSE
                   *> No next node at this level, go to lower level
                   SUBTRACT 1 FROM LEVEL
               END-IF
           END-PERFORM
           .
```

## Key Features of This COBOL Skip List Implementation:

### Data Structure Elements:
- **Max-Level**: Maximum number of levels in the skip list (set to 16)
- **Node Structure**: Each node contains a key value and pointers for each level
- **Head Pointer**: Points to the beginning of the skip list

### Search Algorithm Steps:
1. Start at the highest level
2. Traverse forward while key values are less than search key
3. When key value exceeds search key, drop down to next lower level
4. Continue until key is found or all levels exhausted
5. Return found/not-found status

### Control Flow:
- Uses `PERFORM UNTIL` loops for traversal
- Implements level-by-level navigation
- Handles pointer dereferencing through indexed arrays
- Uses flags to indicate search success/failure

This implementation demonstrates the core skip list search algorithm in COBOL syntax, showing how multi-level linked lists can be implemented using pointers and indexed arrays.