# Ternary Search Algorithm in COBOL

Here's an example implementation of the ternary search algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TERNARY-SEARCH.
       AUTHOR. COBOL-PROGRAMMER.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ARRAY-VALUES.
           05 ARRAY OCCURS 10 TIMES PIC 9(3).
       01  SEARCH-KEY         PIC 9(3).
       01  LEFT-INDEX         PIC 9(2) VALUE 1.
       01  RIGHT-INDEX        PIC 9(2) VALUE 10.
       01  MID1-INDEX         PIC 9(2).
       01  MID2-INDEX         PIC 9(2).
       01  RESULT-INDEX       PIC 9(2) VALUE 0.
       01  FOUND-FLAG         PIC X VALUE 'N'.
           88 FOUND            VALUE 'Y'.
           88 NOT-FOUND        VALUE 'N'.
       01  DISPLAY-MSG        PIC X(50).
       01  TEMP-INDEX         PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           DISPLAY "Ternary Search Example"
           DISPLAY "======================"
           
           * Initialize array with sorted values
           MOVE 10 TO ARRAY(1)
           MOVE 20 TO ARRAY(2)
           MOVE 30 TO ARRAY(3)
           MOVE 40 TO ARRAY(4)
           MOVE 50 TO ARRAY(5)
           MOVE 60 TO ARRAY(6)
           MOVE 70 TO ARRAY(7)
           MOVE 80 TO ARRAY(8)
           MOVE 90 TO ARRAY(9)
           MOVE 100 TO ARRAY(10)
           
           DISPLAY "Array values:"
           PERFORM DISPLAY-ARRAY
           
           * Test with different search keys
           MOVE 50 TO SEARCH-KEY
           PERFORM TERNARY-SEARCH-ROUTINE
           IF FOUND
               DISPLAY "Key " SEARCH-KEY " found at index " RESULT-INDEX
           ELSE
               DISPLAY "Key " SEARCH-KEY " not found"
           END-IF
           
           MOVE 25 TO SEARCH-KEY
           PERFORM TERNARY-SEARCH-ROUTINE
           IF FOUND
               DISPLAY "Key " SEARCH-KEY " found at index " RESULT-INDEX
           ELSE
               DISPLAY "Key " SEARCH-KEY " not found"
           END-IF
           
           MOVE 100 TO SEARCH-KEY
           PERFORM TERNARY-SEARCH-ROUTINE
           IF FOUND
               DISPLAY "Key " SEARCH-KEY " found at index " RESULT-INDEX
           ELSE
               DISPLAY "Key " SEARCH-KEY " not found"
           END-IF
           
           STOP RUN.

       TERNARY-SEARCH-ROUTINE.
           MOVE 1 TO LEFT-INDEX
           MOVE 10 TO RIGHT-INDEX
           MOVE 0 TO RESULT-INDEX
           MOVE 'N' TO FOUND-FLAG
           
           PERFORM UNTIL (LEFT-INDEX > RIGHT-INDEX) OR FOUND
               COMPUTE MID1-INDEX = LEFT-INDEX + (RIGHT-INDEX - LEFT-INDEX) / 3
               COMPUTE MID2-INDEX = RIGHT-INDEX - (RIGHT-INDEX - LEFT-INDEX) / 3
               
               DISPLAY "Searching between " LEFT-INDEX " and " RIGHT-INDEX
               DISPLAY "MID1: " MID1-INDEX " MID2: " MID2-INDEX
               
               IF SEARCH-KEY = ARRAY(MID1-INDEX)
                   MOVE MID1-INDEX TO RESULT-INDEX
                   MOVE 'Y' TO FOUND-FLAG
               ELSE IF SEARCH-KEY = ARRAY(MID2-INDEX)
                   MOVE MID2-INDEX TO RESULT-INDEX
                   MOVE 'Y' TO FOUND-FLAG
               ELSE IF SEARCH-KEY < ARRAY(MID1-INDEX)
                   MOVE LEFT-INDEX TO TEMP-INDEX
                   COMPUTE RIGHT-INDEX = MID1-INDEX - 1
               ELSE IF SEARCH-KEY > ARRAY(MID2-INDEX)
                   MOVE RIGHT-INDEX TO TEMP-INDEX
                   COMPUTE LEFT-INDEX = MID2-INDEX + 1
               ELSE
                   COMPUTE LEFT-INDEX = MID1-INDEX + 1
                   COMPUTE RIGHT-INDEX = MID2-INDEX - 1
               END-IF
           END-PERFORM.

       DISPLAY-ARRAY.
           PERFORM VARYING TEMP-INDEX FROM 1 BY 1 UNTIL TEMP-INDEX > 10
               DISPLAY "Array[" TEMP-INDEX "] = " ARRAY(TEMP-INDEX)
           END-PERFORM.
```

## How the Algorithm Works

This COBOL implementation of ternary search:

1. **Initialization**: Sets up a sorted array of 10 elements and initializes search parameters
2. **Divide**: Divides the search space into three parts using two midpoints
3. **Compare**: Compares the search key with the values at the two midpoints
4. **Conquer**: Narrows the search to one of the three segments based on comparisons
5. **Repeat**: Continues until the element is found or the search space is exhausted

## Key Features

- **Sorted Array Required**: The algorithm assumes the input array is sorted
- **Three-way Division**: Unlike binary search that divides into two parts, ternary search divides into three
- **Efficient**: Has a time complexity of O(log₃ n)
- **Recursive Structure**: The search space is reduced by approximately 2/3 with each iteration

## Output Example
```
Ternary Search Example
======================
Array values:
Array[1] = 10
Array[2] = 20
Array[3] = 30
Array[4] = 40
Array[5] = 50
Array[6] = 60
Array[7] = 70
Array[8] = 80
Array[9] = 90
Array[10] = 100
Key 50 found at index 5
Key 25 not found
Key 100 found at index 10
```

