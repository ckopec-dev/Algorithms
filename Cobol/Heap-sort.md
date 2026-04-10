# Heap Sort Algorithm in COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HEAP-SORT.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01 ARRAY-SIZE        PIC 9(3) VALUE 10.
       01 I                 PIC 9(3).
       01 J                 PIC 9(3).
       01 LARGEST           PIC 9(3).
       01 TEMP              PIC 9(3).
       01 ARRAY-TABLE.
          05 ARRAY-ITEM     PIC 9(3) OCCURS 10 TIMES.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "HEAP SORT EXAMPLE"
           DISPLAY "====================="

           *> Initialize array with sample data
           MOVE 64 TO ARRAY-ITEM(1)
           MOVE 34 TO ARRAY-ITEM(2)
           MOVE 25 TO ARRAY-ITEM(3)
           MOVE 12 TO ARRAY-ITEM(4)
           MOVE 22 TO ARRAY-ITEM(5)
           MOVE 11 TO ARRAY-ITEM(6)
           MOVE 90 TO ARRAY-ITEM(7)
           MOVE 88 TO ARRAY-ITEM(8)
           MOVE 76 TO ARRAY-ITEM(9)
           MOVE 50 TO ARRAY-ITEM(10)

           DISPLAY "Original Array:"
           PERFORM DISPLAY-ARRAY

           *> Perform heap sort
           PERFORM HEAP-SORT-ALGORITHM

           DISPLAY "Sorted Array:"
           PERFORM DISPLAY-ARRAY

           STOP RUN.

       HEAP-SORT-ALGORITHM.
           *> Build max heap
           PERFORM VARYING I FROM (ARRAY-SIZE / 2) BY -1
               UNTIL I < 1
               PERFORM HEAP-FY I
           END-PERFORM

           *> Extract elements from heap one by one
           PERFORM VARYING I FROM ARRAY-SIZE BY -1
               UNTIL I < 2
               *> Move current root to end
               MOVE ARRAY-ITEM(1) TO TEMP
               MOVE ARRAY-ITEM(I) TO ARRAY-ITEM(1)
               MOVE TEMP TO ARRAY-ITEM(I)

               *> Call heapify on the reduced heap
               MOVE 1 TO LARGEST
               PERFORM HEAP-FY 1
           END-PERFORM.

       HEAP-FY.
           *> Heapify function to maintain max heap property
           01 INDEX           PIC 9(3) VALUE 1.
           01 LEFT-CHILD      PIC 9(3).
           01 RIGHT-CHILD     PIC 9(3).
           01 TEMP-SWAP       PIC 9(3).

           MOVE 1 TO INDEX.
           MOVE 1 TO LARGEST.

           PERFORM VARYING J FROM 1 BY 1
               UNTIL J > 3
               IF J = 1
                   COMPUTE LEFT-CHILD = INDEX * 2
                   IF LEFT-CHILD <= ARRAY-SIZE
                       IF ARRAY-ITEM(LEFT-CHILD) > ARRAY-ITEM(LARGEST)
                           MOVE LEFT-CHILD TO LARGEST
                       END-IF
                   END-IF
               ELSE IF J = 2
                   COMPUTE RIGHT-CHILD = INDEX * 2 + 1
                   IF RIGHT-CHILD <= ARRAY-SIZE
                       IF ARRAY-ITEM(RIGHT-CHILD) > ARRAY-ITEM(LARGEST)
                           MOVE RIGHT-CHILD TO LARGEST
                       END-IF
                   END-IF
               ELSE
                   IF LARGEST NOT = INDEX
                       MOVE ARRAY-ITEM(INDEX) TO TEMP-SWAP
                       MOVE ARRAY-ITEM(LARGEST) TO ARRAY-ITEM(INDEX)
                       MOVE TEMP-SWAP TO ARRAY-ITEM(LARGEST)
                       MOVE LARGEST TO INDEX
                   END-IF
               END-IF
           END-PERFORM.

       DISPLAY-ARRAY.
           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > ARRAY-SIZE
               DISPLAY ARRAY-ITEM(I) WITH NO ADVANCING
           END-PERFORM
           DISPLAY SPACE.

       END PROGRAM HEAP-SORT.
```

## Explanation

This COBOL implementation of Heap Sort includes:

1. **Data Division**: Defines the array structure and working storage variables
2. **Main Logic**: Initializes the array with sample data and calls the sorting algorithm
3. **Heap Sort Algorithm**: 
   - Builds a max heap from the input array
   - Repeatedly extracts the maximum element and rebuilds the heap
4. **Heapify Function**: Maintains the max heap property by comparing parent with children
5. **Display Function**: Shows the array before and after sorting

## Key Features

- **Max Heap Construction**: Converts array into max heap structure
- **Heapify Operation**: Ensures parent node is larger than its children
- **Sorting Process**: Repeatedly extracts maximum element to sort the array
- **Complete Implementation**: Includes all necessary COBOL syntax and structure

The algorithm has O(n log n) time complexity and O(1) space complexity, making it efficient for large datasets.

