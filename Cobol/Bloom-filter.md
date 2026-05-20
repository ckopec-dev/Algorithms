# Bloom Filter Implementation in COBOL

Here's an example of a Bloom filter algorithm implemented in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BLOOM-FILTER.
       AUTHOR. AI Assistant.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BLOOM-FILE ASSIGN TO "BLOOM.DAT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD BLOOM-FILE.
       01 BLOOM-RECORD.
          05 BLOOM-BITMAP PIC X(1000).
          05 BLOOM-HASH-VALUES.
             10 HASH-1 PIC 9(4).
             10 HASH-2 PIC 9(4).
             10 HASH-3 PIC 9(4).

       WORKING-STORAGE SECTION.
       01 BLOOM-CONFIG.
          05 BLOOM-SIZE PIC 9(4) VALUE 1000.
          05 HASH-COUNT PIC 9(2) VALUE 3.
          05 BLOOM-BITMAP-ARRAY.
             10 BLOOM-BITMAP-ITEM PIC X OCCURS 1000 TIMES.
       01 HASH-VALUES.
          05 HASH-VALUE-1 PIC 9(4).
          05 HASH-VALUE-2 PIC 9(4).
          05 HASH-VALUE-3 PIC 9(4).
       01 TEMP-VALUES.
          05 TEMP-INDEX PIC 9(4).
          05 TEMP-CHAR PIC X.
          05 TEMP-CHAR-2 PIC X.
       01 INPUT-STRING.
          05 STRING-LENGTH PIC 9(3).
          05 STRING-CONTENT PIC X(100).
       01 RETURN-VALUES.
          05 IS-FOUND PIC 9(1) VALUE 0.
          05 IS-NEW-ITEM PIC 9(1) VALUE 1.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INITIALIZE-BLOOM-FILTER
           PERFORM ADD-ITEMS-TO-BLOOM
           PERFORM TEST-ITEMS-IN-BLOOM
           STOP RUN.

       INITIALIZE-BLOOM-FILTER.
           DISPLAY "Initializing Bloom Filter..."
           MOVE SPACES TO BLOOM-BITMAP-ARRAY
           DISPLAY "Bloom Filter initialized with size " BLOOM-SIZE
           .

       ADD-ITEMS-TO-BLOOM.
           DISPLAY "Adding items to Bloom Filter..."
           
           MOVE "apple" TO STRING-CONTENT
           PERFORM COMPUTE-HASH-VALUES
           PERFORM SET-BITMAP-VALUES
           
           MOVE "banana" TO STRING-CONTENT
           PERFORM COMPUTE-HASH-VALUES
           PERFORM SET-BITMAP-VALUES
           
           MOVE "cherry" TO STRING-CONTENT
           PERFORM COMPUTE-HASH-VALUES
           PERFORM SET-BITMAP-VALUES
           
           DISPLAY "Items added to Bloom Filter"
           .

       COMPUTE-HASH-VALUES.
           MOVE FUNCTION LENGTH(STRING-CONTENT) TO STRING-LENGTH
           COMPUTE HASH-VALUE-1 = FUNCTION MOD(
               FUNCTION HASH-STRING(STRING-CONTENT), BLOOM-SIZE) + 1
           COMPUTE HASH-VALUE-2 = FUNCTION MOD(
               FUNCTION HASH-STRING(STRING-CONTENT) * 23, BLOOM-SIZE) + 1
           COMPUTE HASH-VALUE-3 = FUNCTION MOD(
               FUNCTION HASH-STRING(STRING-CONTENT) * 47, BLOOM-SIZE) + 1
           .

       SET-BITMAP-VALUES.
           MOVE HASH-VALUE-1 TO TEMP-INDEX
           MOVE "1" TO BLOOM-BITMAP-ITEM(TEMP-INDEX)
           
           MOVE HASH-VALUE-2 TO TEMP-INDEX
           MOVE "1" TO BLOOM-BITMAP-ITEM(TEMP-INDEX)
           
           MOVE HASH-VALUE-3 TO TEMP-INDEX
           MOVE "1" TO BLOOM-BITMAP-ITEM(TEMP-INDEX)
           .

       TEST-ITEMS-IN-BLOOM.
           DISPLAY "Testing items in Bloom Filter..."
           
           MOVE "apple" TO STRING-CONTENT
           PERFORM CHECK-ITEM-EXISTENCE
           IF IS-FOUND = 1
               DISPLAY "Apple: EXISTS in Bloom Filter"
           ELSE
               DISPLAY "Apple: NOT FOUND in Bloom Filter"
           END-IF
           
           MOVE "grape" TO STRING-CONTENT
           PERFORM CHECK-ITEM-EXISTENCE
           IF IS-FOUND = 1
               DISPLAY "Grape: EXISTS in Bloom Filter (FALSE POSITIVE!)"
           ELSE
               DISPLAY "Grape: NOT FOUND in Bloom Filter"
           END-IF
           .

       CHECK-ITEM-EXISTENCE.
           PERFORM COMPUTE-HASH-VALUES
           
           MOVE HASH-VALUE-1 TO TEMP-INDEX
           IF BLOOM-BITMAP-ITEM(TEMP-INDEX) = "0"
               MOVE 0 TO IS-FOUND
               GO TO ITEM-FOUND-TEST
           END-IF
           
           MOVE HASH-VALUE-2 TO TEMP-INDEX
           IF BLOOM-BITMAP-ITEM(TEMP-INDEX) = "0"
               MOVE 0 TO IS-FOUND
               GO TO ITEM-FOUND-TEST
           END-IF
           
           MOVE HASH-VALUE-3 TO TEMP-INDEX
           IF BLOOM-BITMAP-ITEM(TEMP-INDEX) = "0"
               MOVE 0 TO IS-FOUND
               GO TO ITEM-FOUND-TEST
           END-IF
           
           MOVE 1 TO IS-FOUND
           
           ITEM-FOUND-TEST.
           .

       FUNCTION HASH-STRING(INPUT-STRING).
           01 HASH-RESULT PIC 9(10) VALUE 0.
           01 I PIC 9(3) VALUE 1.
           01 CHAR-VALUE PIC 9(3) VALUE 0.
           01 TEMP-RESULT PIC 9(10) VALUE 0.
           
           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > FUNCTION LENGTH(INPUT-STRING)
               COMPUTE CHAR-VALUE = FUNCTION ORD(INPUT-STRING(I:1))
               COMPUTE TEMP-RESULT = HASH-RESULT * 31 + CHAR-VALUE
               COMPUTE HASH-RESULT = FUNCTION MOD(TEMP-RESULT, 1000000000)
           END-PERFORM
           RETURN HASH-RESULT
           .
```

## Key Components Explained:

### 1. **Data Structure**
- `BLOOM-BITMAP-ARRAY` - The main Bloom filter array of 1000 bits
- `HASH-VALUES` - Three hash values for the Bloom filter
- `BLOOM-CONFIG` - Configuration parameters

### 2. **Core Functions**

#### **Hash Computation**
- Computes three different hash values using polynomial rolling hash
- Uses different multipliers (23, 47) for better distribution

#### **Adding Items**
- Sets corresponding bits in the bitmap for each hash value
- Uses modulo operation to map hash values to array indices

#### **Membership Testing**
- Computes hash values for the test item
- Checks if all corresponding bits are set
- Returns "true" if all bits are set (potential false positive)

### 3. **Bloom Filter Properties**
- **False Positives**: Possible but no false negatives
- **Space Efficiency**: Uses minimal memory
- **Time Complexity**: O(k) where k is number of hash functions

### 4. **Usage Example**
The program demonstrates:
- Adding "apple", "banana", "cherry" to filter
- Testing "apple" (should return true)
- Testing "grape" (may return false positive)

This implementation shows the fundamental principles of Bloom filters in COBOL, though in practice, modern implementations would use more sophisticated hash functions and potentially better data structures.

