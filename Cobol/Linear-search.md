# Linear Search Algorithm in COBOL

Here's a complete example of a linear search algorithm implemented in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LINEAR-SEARCH.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ARRAY-TABLE.
           05  NUMBERS OCCURS 10 TIMES PIC 9(3).
       01  SEARCH-KEY        PIC 9(3).
       01  INDEX-CTR         PIC 9(2) VALUE 1.
       01  FOUND-FLAG        PIC X VALUE 'N'.
           88  ITEM-FOUND      VALUE 'Y'.
           88  ITEM-NOT-FOUND  VALUE 'N'.
       01  RESULT-MSG        PIC A(50).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Linear Search Algorithm in COBOL"
           DISPLAY "================================"
           
           * Initialize array with sample data
           MOVE 10 TO NUMBERS(1)
           MOVE 25 TO NUMBERS(2)
           MOVE 35 TO NUMBERS(3)
           MOVE 42 TO NUMBERS(4)
           MOVE 58 TO NUMBERS(5)
           MOVE 63 TO NUMBERS(6)
           MOVE 72 TO NUMBERS(7)
           MOVE 85 TO NUMBERS(8)
           MOVE 91 TO NUMBERS(9)
           MOVE 99 TO NUMBERS(10)
           
           DISPLAY "Array elements:"
           PERFORM DISPLAY-ARRAY
           
           * Get search key from user
           DISPLAY "Enter number to search: "
           ACCEPT SEARCH-KEY
           
           * Perform linear search
           PERFORM SEARCH-LOOP UNTIL ITEM-FOUND OR INDEX-CTR > 10
           
           * Display result
           IF ITEM-FOUND
               MOVE "Item found at position " TO RESULT-MSG
               STRING RESULT-MSG INDEX-CTR DELIMITED BY SIZE
               INTO RESULT-MSG
               DISPLAY RESULT-MSG
           ELSE
               DISPLAY "Item not found in the array"
           END-IF
           
           STOP RUN.

       SEARCH-LOOP.
           IF SEARCH-KEY = NUMBERS(INDEX-CTR)
               MOVE 'Y' TO FOUND-FLAG
           ELSE
               ADD 1 TO INDEX-CTR
           END-IF.

       DISPLAY-ARRAY.
           PERFORM VARYING INDEX-CTR FROM 1 BY 1 UNTIL INDEX-CTR > 10
               DISPLAY "Position " INDEX-CTR ": " NUMBERS(INDEX-CTR)
           END-PERFORM.
```

## How it works:

1. **Data Division**: Defines the array `NUMBERS` with 10 elements and variables for search key, index counter, and found flag.

2. **Initialization**: Populates the array with sample data (10, 25, 35, 42, 58, 63, 72, 85, 91, 99).

3. **Search Process**: 
   - Takes user input for the search key
   - Iterates through each element of the array
   - Compares each element with the search key
   - If found, sets the flag and exits loop

4. **Output**: Displays whether the item was found and at which position, or indicates that it wasn't found.

## Sample Output:
```
Linear Search Algorithm in COBOL
================================
Array elements:
Position 1: 10
Position 2: 25
Position 3: 35
Position 4: 42
Position 5: 58
Position 6: 63
Position 7: 72
Position 8: 85
Position 9: 91
Position 10: 99
Enter number to search: 42
Item found at position 4
```

This implementation demonstrates the basic linear search algorithm with O(n) time complexity, checking each element sequentially until the target is found or the end of the array is reached.