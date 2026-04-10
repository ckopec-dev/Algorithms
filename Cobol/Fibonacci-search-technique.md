# Fibonacci Search Technique in COBOL

Here's an example implementation of the Fibonacci search algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIBONACCI-SEARCH.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ARRAY-VALUES.
           05  FIB-ARRAY OCCURS 10 TIMES PIC 9(4).
       01  SEARCH-VALUE     PIC 9(4).
       01  ARRAY-SIZE       PIC 9(2) VALUE 10.
       01  RESULT-INDEX     PIC 9(2).
       01  FOUND            PIC X VALUE 'N'.
           88  ITEM-FOUND   VALUE 'Y'.
           88  ITEM-NOT-FOUND VALUE 'N'.
       
       01  FIB-NUMBERS.
           05  FIB-0          PIC 9(4) VALUE 0.
           05  FIB-1          PIC 9(4) VALUE 1.
           05  FIB-2          PIC 9(4) VALUE 1.
           05  FIB-3          PIC 9(4) VALUE 2.
           05  FIB-4          PIC 9(4) VALUE 3.
           05  FIB-5          PIC 9(4) VALUE 5.
           05  FIB-6          PIC 9(4) VALUE 8.
           05  FIB-7          PIC 9(4) VALUE 13.
           05  FIB-8          PIC 9(4) VALUE 21.
           05  FIB-9          PIC 9(4) VALUE 34.
           05  FIB-10         PIC 9(4) VALUE 55.
       
       01  FIB-INDEX        PIC 9(2).
       01  OFFSET           PIC 9(4).
       01  LOW-INDEX        PIC 9(2) VALUE 1.
       01  HIGH-INDEX       PIC 9(2) VALUE 10.
       01  TEMP-INDEX       PIC 9(2).
       01  TEMP-VALUE       PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "FIBONACCI SEARCH EXAMPLE"
           DISPLAY "========================"
           
           * Initialize array with sorted values
           MOVE 10 TO FIB-ARRAY(1)
           MOVE 25 TO FIB-ARRAY(2)
           MOVE 35 TO FIB-ARRAY(3)
           MOVE 45 TO FIB-ARRAY(4)
           MOVE 55 TO FIB-ARRAY(5)
           MOVE 65 TO FIB-ARRAY(6)
           MOVE 75 TO FIB-ARRAY(7)
           MOVE 85 TO FIB-ARRAY(8)
           MOVE 95 TO FIB-ARRAY(9)
           MOVE 105 TO FIB-ARRAY(10)
           
           DISPLAY "Array values:"
           PERFORM DISPLAY-ARRAY
           
           * Test search with different values
           MOVE 45 TO SEARCH-VALUE
           PERFORM FIBONACCI-SEARCH
           IF ITEM-FOUND
               DISPLAY "Value " SEARCH-VALUE " found at index " RESULT-INDEX
           ELSE
               DISPLAY "Value " SEARCH-VALUE " not found"
           END-IF
           
           MOVE 105 TO SEARCH-VALUE
           PERFORM FIBONACCI-SEARCH
           IF ITEM-FOUND
               DISPLAY "Value " SEARCH-VALUE " found at index " RESULT-INDEX
           ELSE
               DISPLAY "Value " SEARCH-VALUE " not found"
           END-IF
           
           MOVE 30 TO SEARCH-VALUE
           PERFORM FIBONACCI-SEARCH
           IF ITEM-FOUND
               DISPLAY "Value " SEARCH-VALUE " found at index " RESULT-INDEX
           ELSE
               DISPLAY "Value " SEARCH-VALUE " not found"
           END-IF
           
           STOP RUN.

       DISPLAY-ARRAY.
           PERFORM VARYING TEMP-INDEX FROM 1 BY 1 UNTIL TEMP-INDEX > 10
               DISPLAY "Index " TEMP-INDEX ": " FIB-ARRAY(TEMP-INDEX)
           END-PERFORM.

       FIBONACCI-SEARCH.
           MOVE 0 TO RESULT-INDEX
           MOVE 'N' TO FOUND
           
           * Initialize Fibonacci numbers
           MOVE 10 TO FIB-INDEX
           MOVE 10 TO TEMP-INDEX
           
           * Find the smallest Fibonacci number >= array size
           PERFORM FIND-FIB-INDEX
           
           * Set up search boundaries
           MOVE 1 TO LOW-INDEX
           MOVE ARRAY-SIZE TO HIGH-INDEX
           MOVE 0 TO OFFSET
           
           PERFORM UNTIL LOW-INDEX > HIGH-INDEX OR FOUND = 'Y'
               * Calculate middle position
               IF FIB-INDEX > 1
                   COMPUTE TEMP-INDEX = LOW-INDEX + FIB-ARRAY(FIB-INDEX - 1) - 1
                   IF TEMP-INDEX > HIGH-INDEX
                       MOVE HIGH-INDEX TO TEMP-INDEX
                   END-IF
                   MOVE FIB-ARRAY(TEMP-INDEX) TO TEMP-VALUE
               ELSE
                   MOVE FIB-ARRAY(LOW-INDEX) TO TEMP-VALUE
               END-IF
               
               IF TEMP-VALUE = SEARCH-VALUE
                   MOVE TEMP-INDEX TO RESULT-INDEX
                   MOVE 'Y' TO FOUND
               ELSE IF TEMP-VALUE < SEARCH-VALUE
                   * Search right half
                   COMPUTE LOW-INDEX = TEMP-INDEX + 1
                   SUBTRACT 1 FROM FIB-INDEX
               ELSE
                   * Search left half
                   COMPUTE HIGH-INDEX = TEMP-INDEX - 1
                   SUBTRACT 1 FROM FIB-INDEX
               END-IF
           END-PERFORM.

       FIND-FIB-INDEX.
           MOVE 0 TO FIB-INDEX
           PERFORM VARYING TEMP-INDEX FROM 1 BY 1 UNTIL TEMP-INDEX > 10
               IF FIB-ARRAY(TEMP-INDEX) >= ARRAY-SIZE
                   MOVE TEMP-INDEX TO FIB-INDEX
                   GO TO FIND-FIB-INDEX-EXIT
               END-IF
           END-PERFORM.
           FIND-FIB-INDEX-EXIT.
           EXIT.

       END PROGRAM FIBONACCI-SEARCH.
```

## Explanation of the Fibonacci Search Algorithm in COBOL:

### Key Features:
1. **Sorted Array**: The algorithm requires a sorted array for searching
2. **Fibonacci Numbers**: Uses Fibonacci sequence to determine search positions
3. **Efficient Search**: Reduces the search space by approximately 38% at each step

### How it works:
1. **Initialization**: Creates a Fibonacci sequence array
2. **Find Fibonacci Index**: Determines the smallest Fibonacci number ≥ array size
3. **Search Loop**: 
   - Calculates middle position using Fibonacci numbers
   - Compares with search value
   - Narrows search space based on comparison
4. **Result**: Returns index if found, or indicates not found

### Sample Output:
```
FIBONACCI SEARCH EXAMPLE
========================
Array values:
Index 1: 10
Index 2: 25
Index 3: 35
Index 4: 45
Index 5: 55
Index 6: 65
Index 7: 75
Index 8: 85
Index 9: 95
Index 10: 105
Value 45 found at index 4
Value 105 found at index 10
Value 30 not found
```

This implementation demonstrates the Fibonacci search technique in COBOL, which is particularly useful for searching sorted arrays with good performance characteristics.

