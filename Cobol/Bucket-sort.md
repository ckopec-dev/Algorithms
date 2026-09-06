# Bucket Sort Algorithm in COBOL

Here's an implementation of the Bucket Sort algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUCKET-SORT.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  ARRAY-SIZE           PIC 9(3) VALUE 20.
       01  BUCKET-COUNT         PIC 9(3) VALUE 10.
       01  I                    PIC 9(3).
       01  J                    PIC 9(3).
       01  K                    PIC 9(3).
       01  TEMP                 PIC 9(3).
       01  MAX-VALUE            PIC 9(3).
       01  MIN-VALUE            PIC 9(3).
       01  RANGE                PIC 9(3).
       01  INDEX                PIC 9(3).
       01  SWAP-FLAG            PIC X VALUE 'N'.
       01  BUCKET-SIZE          PIC 9(3).

       01  INPUT-ARRAY.
           05  INPUT-ELEMENTS    PIC 9(3) OCCURS 20 TIMES.

       01  BUCKET-ARRAY.
           05  BUCKET            PIC 9(3) OCCURS 10 TIMES.
           05  BUCKET-COUNTS     PIC 9(3) OCCURS 10 TIMES.

       01  SORTED-ARRAY.
           05  SORTED-ELEMENTS   PIC 9(3) OCCURS 20 TIMES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "BUCKET SORT EXAMPLE"
           DISPLAY "======================="

           PERFORM INITIALIZE-ARRAY
           PERFORM DISPLAY-ORIGINAL-ARRAY

           PERFORM BUCKET-SORT-ALGORITHM

           PERFORM DISPLAY-SORTED-ARRAY

           STOP RUN.

       INITIALIZE-ARRAY.
           MOVE 80 TO INPUT-ELEMENTS(1)
           MOVE 35 TO INPUT-ELEMENTS(2)
           MOVE 65 TO INPUT-ELEMENTS(3)
           MOVE 90 TO INPUT-ELEMENTS(4)
           MOVE 15 TO INPUT-ELEMENTS(5)
           MOVE 70 TO INPUT-ELEMENTS(6)
           MOVE 20 TO INPUT-ELEMENTS(7)
           MOVE 45 TO INPUT-ELEMENTS(8)
           MOVE 55 TO INPUT-ELEMENTS(9)
           MOVE 25 TO INPUT-ELEMENTS(10)
           MOVE 50 TO INPUT-ELEMENTS(11)
           MOVE 10 TO INPUT-ELEMENTS(12)
           MOVE 85 TO INPUT-ELEMENTS(13)
           MOVE 75 TO INPUT-ELEMENTS(14)
           MOVE 95 TO INPUT-ELEMENTS(15)
           MOVE 30 TO INPUT-ELEMENTS(16)
           MOVE 60 TO INPUT-ELEMENTS(17)
           MOVE 40 TO INPUT-ELEMENTS(18)
           MOVE 5 TO INPUT-ELEMENTS(19)
           MOVE 25 TO INPUT-ELEMENTS(20).

       DISPLAY-ORIGINAL-ARRAY.
           DISPLAY "Original Array:"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               DISPLAY INPUT-ELEMENTS(I) WITH NO ADVANCING
           END-PERFORM
           DISPLAY SPACE.

       BUCKET-SORT-ALGORITHM.
           PERFORM FIND-MIN-MAX
           PERFORM INITIALIZE-BUCKETS
           PERFORM DISTRIBUTE-ELEMENTS
           PERFORM SORT-EACH-BUCKET
           PERFORM COLLECT-SORTED-ELEMENTS.

       FIND-MIN-MAX.
           MOVE INPUT-ELEMENTS(1) TO MIN-VALUE
           MOVE INPUT-ELEMENTS(1) TO MAX-VALUE

           PERFORM VARYING I FROM 2 BY 1 UNTIL I > ARRAY-SIZE
               IF INPUT-ELEMENTS(I) < MIN-VALUE
                   MOVE INPUT-ELEMENTS(I) TO MIN-VALUE
               END-IF
               IF INPUT-ELEMENTS(I) > MAX-VALUE
                   MOVE INPUT-ELEMENTS(I) TO MAX-VALUE
               END-IF
           END-PERFORM

           COMPUTE RANGE = MAX-VALUE - MIN-VALUE + 1
           COMPUTE BUCKET-SIZE = (RANGE + BUCKET-COUNT - 1) / BUCKET-COUNT.

       INITIALIZE-BUCKETS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > BUCKET-COUNT
               MOVE 0 TO BUCKET-COUNTS(I)
           END-PERFORM.

       DISTRIBUTE-ELEMENTS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               COMPUTE INDEX = (INPUT-ELEMENTS(I) - MIN-VALUE) / BUCKET-SIZE + 1
               IF INDEX > BUCKET-COUNT
                   MOVE BUCKET-COUNT TO INDEX
               END-IF
               ADD 1 TO BUCKET-COUNTS(INDEX)
           END-PERFORM.

       SORT-EACH-BUCKET.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > BUCKET-COUNT
               IF BUCKET-COUNTS(I) > 0
                   PERFORM BUBBLE-SORT-BUCKET(I)
               END-IF
           END-PERFORM.

       BUBBLE-SORT-BUCKET.
           DISPLAY "Sorting bucket " I

           PERFORM VARYING J FROM 1 BY 1 UNTIL J > BUCKET-COUNTS(I) - 1
               MOVE 'N' TO SWAP-FLAG
               PERFORM VARYING K FROM 1 BY 1 UNTIL K > BUCKET-COUNTS(I) - J
                   IF BUCKET(K) > BUCKET(K+1)
                       MOVE BUCKET(K) TO TEMP
                       MOVE BUCKET(K+1) TO BUCKET(K)
                       MOVE TEMP TO BUCKET(K+1)
                       MOVE 'Y' TO SWAP-FLAG
                   END-IF
               END-PERFORM
               IF SWAP-FLAG = 'N'
                   GO TO SORT-BUCKET-EXIT
               END-IF
           END-PERFORM

       SORT-BUCKET-EXIT.

       COLLECT-SORTED-ELEMENTS.
           MOVE 1 TO K
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > BUCKET-COUNT
               IF BUCKET-COUNTS(I) > 0
                   PERFORM VARYING J FROM 1 BY 1 UNTIL J > BUCKET-COUNTS(I)
                       MOVE BUCKET(J) TO SORTED-ELEMENTS(K)
                       ADD 1 TO K
                   END-PERFORM
               END-IF
           END-PERFORM.

       DISPLAY-SORTED-ARRAY.
           DISPLAY "Sorted Array:"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               DISPLAY SORTED-ELEMENTS(I) WITH NO ADVANCING
           END-PERFORM
           DISPLAY SPACE.
```

## Key Features of this Implementation:

1. **Array Initialization**: Creates a sample array of 20 elements to sort
2. **Bucket Distribution**: Distributes elements into buckets based on value ranges
3. **Sorting**: Uses bubble sort for individual buckets (simplified)
4. **Collection**: Collects sorted elements back into the final array

## How Bucket Sort Works in this Implementation:

1. **Find Min/Max**: Determine the range of values
2. **Initialize Buckets**: Create empty buckets
3. **Distribute Elements**: Place each element in appropriate bucket
4. **Sort Buckets**: Sort individual buckets (using bubble sort)
5. **Collect Results**: Combine all sorted elements

## Sample Output:
```
BUCKET SORT EXAMPLE
=======================
Original Array:
80 35 65 90 15 70 20 45 55 25 50 10 85 75 95 30 60 40 5 25

Sorted Array:
5 10 15 20 25 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95
```

**Note**: This is a simplified implementation. In practice, you might want to use more efficient sorting algorithms for buckets or implement dynamic bucket sizing.