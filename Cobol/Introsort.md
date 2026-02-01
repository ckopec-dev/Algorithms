# Introsort Algorithm in COBOL

Here's an example implementation of the Introsort algorithm in COBOL. This implementation includes the hybrid sorting approach that combines quicksort, heapsort, and insertion sort.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTROSORT-EXAMPLE.
       AUTHOR. COBOL-DEVELOPER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SORT-FILE ASSIGN TO SORT-DATA
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD SORT-FILE.
       01 SORT-RECORD.
          05 RECORD-KEY    PIC 9(8).
          05 RECORD-DATA   PIC X(20).

       WORKING-STORAGE SECTION.
       01 SORT-ARRAY.
          05 ARRAY-ELEMENTS PIC 9(8) OCCURS 100 TIMES.
       01 SORT-LENGTH    PIC 9(3) VALUE 100.
       01 DEPTH-LIMIT    PIC 9(3).
       01 TEMP-VALUE     PIC 9(8).
       01 I-INDEX        PIC 9(3).
       01 J-INDEX        PIC 9(3).
       01 PIVOT-VALUE    PIC 9(8).
       01 HEAP-SIZE      PIC 9(3).
       01 MAX-DEPTH      PIC 9(3) VALUE 14.
       01 INSERTION-SIZE PIC 9(3) VALUE 10.
       01 DEBUG-FLAG     PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "INTROSORT ALGORITHM EXAMPLE"
           DISPLAY "============================"
           
           PERFORM INITIALIZE-ARRAY
           PERFORM DISPLAY-ARRAY "BEFORE SORTING"
           
           PERFORM INTROSORT-DRIVER
           
           PERFORM DISPLAY-ARRAY "AFTER SORTING"
           
           STOP RUN.

       INITIALIZE-ARRAY.
           MOVE 100 TO SORT-LENGTH
           MOVE 14 TO DEPTH-LIMIT
           
           *> Initialize array with random values
           PERFORM VARYING I-INDEX FROM 1 BY 1 UNTIL I-INDEX > SORT-LENGTH
               COMPUTE ARRAY-ELEMENTS(I-INDEX) = FUNCTION RANDOM * 1000
           END-PERFORM.

       INTROSORT-DRIVER.
           *> Call the main introsort function
           CALL "INTROSORT" USING SORT-ARRAY, SORT-LENGTH, DEPTH-LIMIT.

       INTROSORT.
           *> Introsort main function
           PROCEDURE DIVISION USING SORT-ARRAY, LENGTH, DEPTH-LIMIT.
           01 I-INDEX        PIC 9(3).
           01 J-INDEX        PIC 9(3).
           01 TEMP-VALUE     PIC 9(8).
           
           *> Check if depth limit reached
           IF DEPTH-LIMIT < 0 THEN
               PERFORM HEAPSORT USING SORT-ARRAY, LENGTH
               GO TO INTROSORT-END
           END-IF.
           
           *> Check if array size is small, use insertion sort
           IF LENGTH <= INSERTION-SIZE THEN
               PERFORM INSERTION-SORT USING SORT-ARRAY, LENGTH
               GO TO INTROSORT-END
           END-IF.
           
           *> Use quicksort with median-of-three pivot
           PERFORM QUICKSORT USING SORT-ARRAY, LENGTH, DEPTH-LIMIT.
           
           INTROSORT-END.
           EXIT.

       QUICKSORT.
           *> Quicksort implementation with median-of-three pivot
           PROCEDURE DIVISION USING SORT-ARRAY, LENGTH, DEPTH-LIMIT.
           01 LOW            PIC 9(3) VALUE 1.
           01 HIGH           PIC 9(3).
           01 I-INDEX        PIC 9(3).
           01 J-INDEX        PIC 9(3).
           01 PIVOT-VALUE    PIC 9(8).
           01 TEMP-VALUE     PIC 9(8).
           
           COMPUTE HIGH = LENGTH
           
           *> Partition the array
           PERFORM PARTITION USING SORT-ARRAY, LOW, HIGH, PIVOT-VALUE
           MOVE PIVOT-VALUE TO TEMP-VALUE
           
           *> Recursively sort left and right partitions
           IF LOW < HIGH THEN
               COMPUTE I-INDEX = HIGH - 1
               PERFORM QUICKSORT USING SORT-ARRAY, I-INDEX, DEPTH-LIMIT
               
               COMPUTE I-INDEX = HIGH + 1
               COMPUTE J-INDEX = LENGTH
               PERFORM QUICKSORT USING SORT-ARRAY, J-INDEX, DEPTH-LIMIT
           END-IF.

       PARTITION.
           *> Partition function for quicksort
           PROCEDURE DIVISION USING SORT-ARRAY, LOW, HIGH, PIVOT-VALUE.
           01 I-INDEX        PIC 9(3).
           01 J-INDEX        PIC 9(3).
           01 TEMP-VALUE     PIC 9(8).
           01 PIVOT          PIC 9(8).
           01 PIVOT-INDEX    PIC 9(3).
           
           *> Median-of-three pivot selection
           PERFORM MEDIAN-OF-THREE USING SORT-ARRAY, LOW, HIGH, PIVOT-INDEX
           MOVE ARRAY-ELEMENTS(PIVOT-INDEX) TO PIVOT
           
           MOVE LOW TO I-INDEX
           MOVE HIGH TO J-INDEX
           
           *> Partition loop
           PERFORM UNTIL I-INDEX >= J-INDEX
               PERFORM UNTIL ARRAY-ELEMENTS(I-INDEX) >= PIVOT
                   ADD 1 TO I-INDEX
               END-PERFORM
               
               PERFORM UNTIL ARRAY-ELEMENTS(J-INDEX) <= PIVOT
                   SUBTRACT 1 FROM J-INDEX
               END-PERFORM
               
               IF I-INDEX <= J-INDEX THEN
                   *> Swap elements
                   MOVE ARRAY-ELEMENTS(I-INDEX) TO TEMP-VALUE
                   MOVE ARRAY-ELEMENTS(J-INDEX) TO ARRAY-ELEMENTS(I-INDEX)
                   MOVE TEMP-VALUE TO ARRAY-ELEMENTS(J-INDEX)
                   
                   ADD 1 TO I-INDEX
                   SUBTRACT 1 FROM J-INDEX
               END-IF
           END-PERFORM.
           
           MOVE J-INDEX TO PIVOT-VALUE.

       MEDIAN-OF-THREE.
           *> Find median of first, middle, and last elements
           PROCEDURE DIVISION USING SORT-ARRAY, LOW, HIGH, PIVOT-INDEX.
           01 MIDDLE         PIC 9(3).
           01 TEMP-VALUE     PIC 9(8).
           01 A-VALUE        PIC 9(8).
           01 B-VALUE        PIC 9(8).
           01 C-VALUE        PIC 9(8).
           
           COMPUTE MIDDLE = (LOW + HIGH) / 2
           
           MOVE ARRAY-ELEMENTS(LOW) TO A-VALUE
           MOVE ARRAY-ELEMENTS(MIDDLE) TO B-VALUE
           MOVE ARRAY-ELEMENTS(HIGH) TO C-VALUE
           
           *> Simple median finding logic
           IF A-VALUE <= B-VALUE AND B-VALUE <= C-VALUE THEN
               MOVE MIDDLE TO PIVOT-INDEX
           ELSE IF B-VALUE <= A-VALUE AND A-VALUE <= C-VALUE THEN
               MOVE LOW TO PIVOT-INDEX
           ELSE
               MOVE HIGH TO PIVOT-INDEX
           END-IF.

       INSERTION-SORT.
           *> Insertion sort for small arrays
           PROCEDURE DIVISION USING SORT-ARRAY, LENGTH.
           01 I-INDEX        PIC 9(3).
           01 J-INDEX        PIC 9(3).
           01 TEMP-VALUE     PIC 9(8).
           
           PERFORM VARYING I-INDEX FROM 2 BY 1 UNTIL I-INDEX > LENGTH
               MOVE ARRAY-ELEMENTS(I-INDEX) TO TEMP-VALUE
               MOVE I-INDEX TO J-INDEX
               
               PERFORM UNTIL J-INDEX <= 1 OR ARRAY-ELEMENTS(J-INDEX - 1) <= TEMP-VALUE
                   MOVE ARRAY-ELEMENTS(J-INDEX - 1) TO ARRAY-ELEMENTS(J-INDEX)
                   SUBTRACT 1 FROM J-INDEX
               END-PERFORM
               
               MOVE TEMP-VALUE TO ARRAY-ELEMENTS(J-INDEX)
           END-PERFORM.

       HEAPSORT.
           *> Heapsort implementation for when depth limit is reached
           PROCEDURE DIVISION USING SORT-ARRAY, LENGTH.
           01 I-INDEX        PIC 9(3).
           01 HEAP-SIZE      PIC 9(3).
           
           MOVE LENGTH TO HEAP-SIZE
           
           *> Build max heap
           PERFORM VARYING I-INDEX FROM HEAP-SIZE / 2 BY -1 UNTIL I-INDEX < 1
               PERFORM HEAP-FY USING SORT-ARRAY, HEAP-SIZE, I-INDEX
           END-PERFORM.
           
           *> Extract elements from heap
           PERFORM VARYING I-INDEX FROM HEAP-SIZE BY -1 UNTIL I-INDEX < 2
               *> Swap root with last element
               MOVE ARRAY-ELEMENTS(1) TO TEMP-VALUE
               MOVE ARRAY-ELEMENTS(I-INDEX) TO ARRAY-ELEMENTS(1)
               MOVE TEMP-VALUE TO ARRAY-ELEMENTS(I-INDEX)
               
               *> Heapify reduced heap
               PERFORM HEAP-FY USING SORT-ARRAY, I-INDEX - 1, 1
           END-PERFORM.

       HEAP-FY.
           *> Heapify function for heapsort
           PROCEDURE DIVISION USING SORT-ARRAY, HEAP-SIZE, ROOT-INDEX.
           01 L-INDEX        PIC 9(3).
           01 R-INDEX        PIC 9(3).
           01 LARGEST        PIC 9(3).
           01 TEMP-VALUE     PIC 9(8).
           
           MOVE ROOT-INDEX TO LARGEST
           COMPUTE L-INDEX = 2 * ROOT-INDEX
           COMPUTE R-INDEX = 2 * ROOT-INDEX + 1
           
           *> Find largest among root, left, and right
           IF L-INDEX <= HEAP-SIZE AND ARRAY-ELEMENTS(L-INDEX) > ARRAY-ELEMENTS(LARGEST)
               MOVE L-INDEX TO LARGEST
           END-IF.
           
           IF R-INDEX <= HEAP-SIZE AND ARRAY-ELEMENTS(R-INDEX) > ARRAY-ELEMENTS(LARGEST)
               MOVE R-INDEX TO LARGEST
           END-IF.
           
           *> If largest is not root
           IF LARGEST NOT = ROOT-INDEX THEN
               MOVE ARRAY-ELEMENTS(ROOT-INDEX) TO TEMP-VALUE
               MOVE ARRAY-ELEMENTS(LARGEST) TO ARRAY-ELEMENTS(ROOT-INDEX)
               MOVE TEMP-VALUE TO ARRAY-ELEMENTS(LARGEST)
               
               *> Recursively heapify the affected sub-tree
               PERFORM HEAP-FY USING SORT-ARRAY, HEAP-SIZE, LARGEST
           END-IF.

       DISPLAY-ARRAY.
           *> Display array elements
           PROCEDURE DIVISION USING DISPLAY-TYPE.
           01 DISPLAY-INDEX  PIC 9(3).
           01 DISPLAY-COUNT  PIC 9(3) VALUE 0.
           
           DISPLAY DISPLAY-TYPE
           DISPLAY "------------------------"
           
           PERFORM VARYING DISPLAY-INDEX FROM 1 BY 1 UNTIL DISPLAY-INDEX > SORT-LENGTH
               DISPLAY "Element " DISPLAY-INDEX ": " ARRAY-ELEMENTS(DISPLAY-INDEX)
               ADD 1 TO DISPLAY-COUNT
               IF DISPLAY-COUNT > 20 THEN
                   DISPLAY "..."
                   GO TO DISPLAY-END
               END-IF
           END-PERFORM.
           
           DISPLAY-END.
           EXIT.
```

## Key Features of This Introsort Implementation:

1. **Hybrid Approach**: Combines quicksort, heapsort, and insertion sort
2. **Depth Limit**: Uses a maximum recursion depth to switch to heapsort
3. **Median-of-Three Pivot**: Improves quicksort performance
4. **Insertion Sort**: Used for small arrays (≤ 10 elements)
5. **Heap Sort**: Called when depth limit is reached

## Algorithm Steps:

1. **Check Depth Limit**: If maximum depth reached, switch to heapsort
2. **Small Array Check**: If array size ≤ 10, use insertion sort
3. **Quicksort Phase**: Use quicksort with median-of-three pivot
4. **Recursion**: Continue with partitioned subarrays
5. **Fallback**: Switch to heapsort when depth limit exceeded

This implementation demonstrates how COBOL can be used to implement complex algorithms while maintaining readability and structure.

