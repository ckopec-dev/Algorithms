# Cuckoo Hashing Algorithm in COBOL

Below is an example implementation of the Cuckoo hashing algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUCKOO-HASHING.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT HASH-FILE ASSIGN TO "HASH-DATA.DAT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD HASH-FILE.
       01 HASH-RECORD.
          05 HASH-KEY      PIC 9(8).
          05 HASH-VALUE    PIC X(20).
          05 HASH-STATUS   PIC X(1) VALUE 'A'.

       WORKING-STORAGE SECTION.
       01 HASH-TABLE-1.
          05 HT1-ENTRY OCCURS 101 TIMES INDEXED BY HT1-INDEX.
             10 HT1-KEY    PIC 9(8).
             10 HT1-VALUE  PIC X(20).
             10 HT1-VALID  PIC X(1) VALUE 'N'.

       01 HASH-TABLE-2.
          05 HT2-ENTRY OCCURS 101 TIMES INDEXED BY HT2-INDEX.
             10 HT2-KEY    PIC 9(8).
             10 HT2-VALUE  PIC X(20).
             10 HT2-VALID  PIC X(1) VALUE 'N'.

       01 HASH-STATUS-FLAG PIC X(1) VALUE 'N'.
       01 HASH-ATTEMPTS    PIC 9(3) VALUE 0.
       01 HASH-MAX-ATTEMPTS PIC 9(3) VALUE 10.
       01 HASH-INDEX-1     PIC 9(3).
       01 HASH-INDEX-2     PIC 9(3).
       01 HASH-TEMP-KEY    PIC 9(8).
       01 HASH-TEMP-VALUE  PIC X(20).
       01 HASH-TEMP-VALID  PIC X(1).

       01 WORK-RECORD.
          05 W-KEY      PIC 9(8).
          05 W-VALUE    PIC X(20).
          05 W-STATUS   PIC X(1).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "CUCKOO HASHING DEMONSTRATION"
           DISPLAY "============================"

           PERFORM INITIALIZE-HASH-TABLES
           PERFORM INSERT-TEST-DATA
           PERFORM SEARCH-TESTS
           PERFORM DISPLAY-RESULTS

           STOP RUN.

       INITIALIZE-HASH-TABLES.
           MOVE ZERO TO HT1-INDEX
           PERFORM VARYING HT1-INDEX FROM 1 BY 1
               UNTIL HT1-INDEX > 101
               MOVE SPACES TO HT1-ENTRY(HT1-INDEX)
               MOVE 'N' TO HT1-VALID(HT1-INDEX)
           END-PERFORM

           MOVE ZERO TO HT2-INDEX
           PERFORM VARYING HT2-INDEX FROM 1 BY 1
               UNTIL HT2-INDEX > 101
               MOVE SPACES TO HT2-ENTRY(HT2-INDEX)
               MOVE 'N' TO HT2-VALID(HT2-INDEX)
           END-PERFORM.

       INSERT-TEST-DATA.
           DISPLAY "Inserting test data..."
           PERFORM INSERT-KEY-VALUE USING 1001, "ALICE"
           PERFORM INSERT-KEY-VALUE USING 2002, "BOB"
           PERFORM INSERT-KEY-VALUE USING 3003, "CHARLIE"
           PERFORM INSERT-KEY-VALUE USING 4004, "DAVID"
           PERFORM INSERT-KEY-VALUE USING 5005, "EVE"
           PERFORM INSERT-KEY-VALUE USING 6006, "FRANK"
           PERFORM INSERT-KEY-VALUE USING 7007, "GRACE"
           PERFORM INSERT-KEY-VALUE USING 8008, "HENRY"
           PERFORM INSERT-KEY-VALUE USING 9009, "IRENE"
           PERFORM INSERT-KEY-VALUE USING 10010, "JACK"

           DISPLAY "Insertion completed."

       INSERT-KEY-VALUE.
           *> This is the main Cuckoo hashing insertion algorithm
           *> Parameters: W-KEY and W-VALUE
           *> Returns: Success or failure status

           MOVE FUNCTION MOD(W-KEY, 101) TO HASH-INDEX-1
           ADD 1 TO HASH-INDEX-1
           MOVE FUNCTION MOD(W-KEY * 31, 101) TO HASH-INDEX-2
           ADD 1 TO HASH-INDEX-2

           MOVE W-KEY TO HASH-TEMP-KEY
           MOVE W-VALUE TO HASH-TEMP-VALUE
           MOVE 'Y' TO HASH-TEMP-VALID

           MOVE ZERO TO HASH-ATTEMPTS
           PERFORM CUCKOO-INSERT-LOOP UNTIL HASH-ATTEMPTS > HASH-MAX-ATTEMPTS
               OR HASH-STATUS-FLAG = 'S'

           IF HASH-ATTEMPTS > HASH-MAX-ATTEMPTS
               DISPLAY "Cuckoo hashing failed - max attempts exceeded"
               MOVE 'F' TO HASH-STATUS-FLAG
           ELSE
               DISPLAY "Key " W-KEY " inserted successfully"
           END-IF.

       CUCKOO-INSERT-LOOP.
           ADD 1 TO HASH-ATTEMPTS

           *> Try to insert in first hash table
           IF HT1-VALID(HASH-INDEX-1) = 'N'
               MOVE HASH-TEMP-KEY TO HT1-KEY(HASH-INDEX-1)
               MOVE HASH-TEMP-VALUE TO HT1-VALUE(HASH-INDEX-1)
               MOVE 'Y' TO HT1-VALID(HASH-INDEX-1)
               MOVE 'S' TO HASH-STATUS-FLAG
               GO TO CUCKOO-END-INSERT
           END-IF

           *> Try to insert in second hash table
           IF HT2-VALID(HASH-INDEX-2) = 'N'
               MOVE HASH-TEMP-KEY TO HT2-KEY(HASH-INDEX-2)
               MOVE HASH-TEMP-VALUE TO HT2-VALUE(HASH-INDEX-2)
               MOVE 'Y' TO HT2-VALID(HASH-INDEX-2)
               MOVE 'S' TO HASH-STATUS-FLAG
               GO TO CUCKOO-END-INSERT
           END-IF

           *> Cuckoo: Evict existing element and try to rehash it
           *> Evict from first table
           MOVE HT1-KEY(HASH-INDEX-1) TO HASH-TEMP-KEY
           MOVE HT1-VALUE(HASH-INDEX-1) TO HASH-TEMP-VALUE

           *> Remove from first table
           MOVE SPACES TO HT1-KEY(HASH-INDEX-1)
           MOVE SPACES TO HT1-VALUE(HASH-INDEX-1)
           MOVE 'N' TO HT1-VALID(HASH-INDEX-1)

           *> Find new position for evicted element
           MOVE FUNCTION MOD(HASH-TEMP-KEY, 101) TO HASH-INDEX-1
           ADD 1 TO HASH-INDEX-1
           MOVE FUNCTION MOD(HASH-TEMP-KEY * 31, 101) TO HASH-INDEX-2
           ADD 1 TO HASH-INDEX-2

           *> Continue with the evicted element in second table
           IF HT2-VALID(HASH-INDEX-2) = 'N'
               MOVE HASH-TEMP-KEY TO HT2-KEY(HASH-INDEX-2)
               MOVE HASH-TEMP-VALUE TO HT2-VALUE(HASH-INDEX-2)
               MOVE 'Y' TO HT2-VALID(HASH-INDEX-2)
               MOVE 'S' TO HASH-STATUS-FLAG
           END-IF.

       CUCKOO-END-INSERT.
           CONTINUE.

       SEARCH-TESTS.
           DISPLAY "Searching for keys..."
           PERFORM SEARCH-KEY USING 1001
           PERFORM SEARCH-KEY USING 3003
           PERFORM SEARCH-KEY USING 5005
           PERFORM SEARCH-KEY USING 9009
           PERFORM SEARCH-KEY USING 10000.

       SEARCH-KEY.
           *> Search for a key in the hash tables
           MOVE FUNCTION MOD(W-KEY, 101) TO HASH-INDEX-1
           ADD 1 TO HASH-INDEX-1
           MOVE FUNCTION MOD(W-KEY * 31, 101) TO HASH-INDEX-2
           ADD 1 TO HASH-INDEX-2

           IF HT1-VALID(HASH-INDEX-1) = 'Y' AND HT1-KEY(HASH-INDEX-1) = W-KEY
               DISPLAY "Found key " W-KEY " in table 1: " HT1-VALUE(HASH-INDEX-1)
           ELSE IF HT2-VALID(HASH-INDEX-2) = 'Y' AND HT2-KEY(HASH-INDEX-2) = W-KEY
               DISPLAY "Found key " W-KEY " in table 2: " HT2-VALUE(HASH-INDEX-2)
           ELSE
               DISPLAY "Key " W-KEY " not found"
           END-IF.

       DISPLAY-RESULTS.
           DISPLAY "Final Hash Table Contents:"
           DISPLAY "=========================="

           MOVE ZERO TO HT1-INDEX
           PERFORM VARYING HT1-INDEX FROM 1 BY 1
               UNTIL HT1-INDEX > 101
               IF HT1-VALID(HT1-INDEX) = 'Y'
                   DISPLAY "Table 1 - Index: " HT1-INDEX
                   DISPLAY "  Key: " HT1-KEY(HT1-INDEX)
                   DISPLAY "  Value: " HT1-VALUE(HT1-INDEX)
               END-IF
           END-PERFORM

           MOVE ZERO TO HT2-INDEX
           PERFORM VARYING HT2-INDEX FROM 1 BY 1
               UNTIL HT2-INDEX > 101
               IF HT2-VALID(HT2-INDEX) = 'Y'
                   DISPLAY "Table 2 - Index: " HT2-INDEX
                   DISPLAY "  Key: " HT2-KEY(HT2-INDEX)
                   DISPLAY "  Value: " HT2-VALUE(HT2-INDEX)
               END-IF
           END-PERFORM.
```

## Key Features of This Implementation:

### 1. **Two Hash Tables**
- `HASH-TABLE-1` and `HASH-TABLE-2` - Two separate arrays for storing hash entries
- Each table has 101 slots (prime number for better distribution)

### 2. **Hash Functions**
- Uses modulo operations with different prime numbers (101 and 31)
- First hash: `key MOD 101`
- Second hash: `(key * 31) MOD 101`

### 3. **Cuckoo Insertion Algorithm**
- Attempts to insert in first table
- If occupied, tries second table
- If both occupied, evicts an element and rehashes it
- Maximum attempts limited to prevent infinite loops

### 4. **Collision Resolution**
- Implements traditional cuckoo hashing with two hash functions
- Uses "cuckoo" eviction mechanism where existing elements are moved to alternative positions

### 5. **Search Functionality**
- Searches both tables using the same hash functions
- Returns found values or indicates key not found

This implementation demonstrates the core concepts of cuckoo hashing in COBOL, though it's worth noting that COBOL is not typically used for such algorithms due to its procedural nature and lack of modern data structure support.