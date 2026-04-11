# Exponential Search Algorithm in COBOL

Here's an example implementation of the Exponential Search algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXPONENTIAL-SEARCH.
       AUTHOR. COBOL EXAMPLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ARRAY-SIZE        PIC 9(3) VALUE 100.
       01  SEARCH-VALUE      PIC 9(5).
       01  RESULT            PIC 9(5).
       01  LOW               PIC 9(5) VALUE 0.
       01  HIGH              PIC 9(5) VALUE 0.
       01  INDEX             PIC 9(5) VALUE 0.
       01  EXPONENT          PIC 9(5) VALUE 1.
       01  FOUND             PIC X VALUE 'N'.
       01  ARRAY-ITEM        PIC 9(5).
       
       01  ARRAY-TABLE.
           05  ARRAY-ITEMS       OCCURS 100 TIMES PIC 9(5).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-ARRAY
           DISPLAY "Array initialized with 100 elements"
           
           DISPLAY "Enter value to search: "
           ACCEPT SEARCH-VALUE
           
           PERFORM EXPONENTIAL-SEARCH-ALGORITHM
           
           IF FOUND = 'Y'
               DISPLAY "Value " SEARCH-VALUE " found at position " RESULT
           ELSE
               DISPLAY "Value " SEARCH-VALUE " not found in array"
           END-IF
           
           STOP RUN.

       INITIALIZE-ARRAY.
           PERFORM VARYING INDEX FROM 1 BY 1 UNTIL INDEX > 100
               MOVE INDEX TO ARRAY-ITEMS(INDEX)
           END-PERFORM.

       EXPONENTIAL-SEARCH-ALGORITHM.
           MOVE 0 TO RESULT
           MOVE 'N' TO FOUND
           
           IF SEARCH-VALUE = ARRAY-ITEMS(1)
               MOVE 1 TO RESULT
               MOVE 'Y' TO FOUND
               GO TO RETURN-RESULT
           END-IF
           
           MOVE 1 TO EXPONENT
           MOVE 1 TO INDEX
           
           PERFORM UNTIL INDEX >= ARRAY-SIZE OR SEARCH-VALUE < ARRAY-ITEMS(INDEX)
               MULTIPLY EXPONENT BY 2 GIVING INDEX
               ADD 1 TO EXPONENT
           END-PERFORM
           
           IF INDEX > ARRAY-SIZE
               MOVE ARRAY-SIZE TO HIGH
           ELSE
               MOVE INDEX TO HIGH
           END-IF
           
           MOVE 1 TO LOW
           
           PERFORM BINARY-SEARCH
           
           GO TO RETURN-RESULT.

       BINARY-SEARCH.
           PERFORM UNTIL LOW > HIGH
               COMPUTE INDEX = (LOW + HIGH) / 2
               MOVE ARRAY-ITEMS(INDEX) TO ARRAY-ITEM
               
               IF SEARCH-VALUE = ARRAY-ITEM
                   MOVE INDEX TO RESULT
                   MOVE 'Y' TO FOUND
                   GO TO RETURN-RESULT
               ELSE IF SEARCH-VALUE < ARRAY-ITEM
                   COMPUTE HIGH = INDEX - 1
               ELSE
                   COMPUTE LOW = INDEX + 1
               END-IF
           END-PERFORM.

       RETURN-RESULT.
           EXIT.
```

## Algorithm Explanation

This COBOL implementation of exponential search follows these steps:

1. **Initial Check**: First checks if the target value is at the first position
2. **Exponential Growth**: Doubles the search range until finding an element greater than or equal to the target
3. **Binary Search**: Performs binary search within the identified range

## Key Features

- Uses an array of 100 elements initialized with sequential values (1-100)
- Handles edge cases like searching for the first element
- Implements both exponential growth and binary search components
- Returns the position (1-based) of the found element or indicates not found

## Time Complexity
- **Best Case**: O(1) - when element is found at first position
- **Average/Worst Case**: O(log n) - where n is the number of elements

## Space Complexity
- O(1) - only uses a constant amount of extra space

The algorithm is particularly efficient for unbounded or very large arrays where the target element is likely to be near the beginning.

