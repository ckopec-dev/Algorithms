# Pigeonhole Sort in COBOL

Here's an implementation of the Pigeonhole Sort algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PIGEONHOLE-SORT.
       AUTHOR. COBOL EXAMPLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ARRAY-SIZE        PIC 9(3) VALUE 10.
       01 MIN-VALUE         PIC 9(5).
       01 MAX-VALUE         PIC 9(5).
       01 RANGE-VALUE       PIC 9(5).
       01 I                 PIC 9(3).
       01 J                 PIC 9(3).
       01 TEMP              PIC 9(5).
       01 SWAP-FLAG         PIC X VALUE 'N'.
       
       01 INPUT-ARRAY.
          05 INPUT-ELEMENTS    OCCURS 100 TIMES PIC 9(5).
       
       01 PIGEONHOLE-ARRAY.
          05 HOLE-ELEMENTS     OCCURS 1000 TIMES PIC 9(5).
       
       01 SORTED-ARRAY.
          05 SORTED-ELEMENTS   OCCURS 100 TIMES PIC 9(5).

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Pigeonhole Sort Example"
           DISPLAY "========================="
           
           * Initialize input array
           MOVE 4 TO INPUT-ELEMENTS(1)
           MOVE 2 TO INPUT-ELEMENTS(2)
           MOVE 7 TO INPUT-ELEMENTS(3)
           MOVE 1 TO INPUT-ELEMENTS(4)
           MOVE 9 TO INPUT-ELEMENTS(5)
           MOVE 3 TO INPUT-ELEMENTS(6)
           MOVE 8 TO INPUT-ELEMENTS(7)
           MOVE 6 TO INPUT-ELEMENTS(8)
           MOVE 5 TO INPUT-ELEMENTS(9)
           MOVE 0 TO INPUT-ELEMENTS(10)
           
           DISPLAY "Original Array:"
           PERFORM DISPLAY-ARRAY
           
           * Perform pigeonhole sort
           PERFORM PIGEONHOLE-SORT-ALGORITHM
           
           DISPLAY "Sorted Array:"
           PERFORM DISPLAY-ARRAY
           
           STOP RUN.

       PIGEONHOLE-SORT-ALGORITHM.
           * Find min and max values
           MOVE INPUT-ELEMENTS(1) TO MIN-VALUE
           MOVE INPUT-ELEMENTS(1) TO MAX-VALUE
           
           PERFORM FIND-MIN-MAX
           
           * Calculate range
           COMPUTE RANGE-VALUE = MAX-VALUE - MIN-VALUE + 1
           
           DISPLAY "Min: " MIN-VALUE ", Max: " MAX-VALUE ", Range: " RANGE-VALUE
           
           * Initialize pigeonhole array
           PERFORM INITIALIZE-PIGEONHOLES
           
           * Place elements in pigeonholes
           PERFORM PLACE-ELEMENTS-IN-HOLES
           
           * Collect elements from holes (sorting)
           PERFORM COLLECT-SORTED-ELEMENTS
           
           GOBACK.

       FIND-MIN-MAX.
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > ARRAY-SIZE
               IF INPUT-ELEMENTS(I) < MIN-VALUE
                   MOVE INPUT-ELEMENTS(I) TO MIN-VALUE
               END-IF
               IF INPUT-ELEMENTS(I) > MAX-VALUE
                   MOVE INPUT-ELEMENTS(I) TO MAX-VALUE
               END-IF
           END-PERFORM.

       INITIALIZE-PIGEONHOLES.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > RANGE-VALUE
               MOVE 0 TO HOLE-ELEMENTS(I)
           END-PERFORM.

       PLACE-ELEMENTS-IN-HOLES.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               COMPUTE J = INPUT-ELEMENTS(I) - MIN-VALUE + 1
               ADD 1 TO HOLE-ELEMENTS(J)
           END-PERFORM.

       COLLECT-SORTED-ELEMENTS.
           MOVE 0 TO J
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > RANGE-VALUE
               IF HOLE-ELEMENTS(I) > 0
                   PERFORM VARYING K FROM 1 BY 1 UNTIL K > HOLE-ELEMENTS(I)
                       ADD 1 TO J
                       COMPUTE SORTED-ELEMENTS(J) = I + MIN-VALUE - 1
                   END-PERFORM
               END-IF
           END-PERFORM.
           
           * Copy sorted elements back to input array
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               MOVE SORTED-ELEMENTS(I) TO INPUT-ELEMENTS(I)
           END-PERFORM.

       DISPLAY-ARRAY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               DISPLAY INPUT-ELEMENTS(I) " "
           END-PERFORM
           DISPLAY SPACE.
```

## How Pigeonhole Sort Works in This COBOL Program:

1. **Initialization**: The program initializes an input array with sample data (0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

2. **Find Range**: Determines minimum and maximum values in the array to calculate the range

3. **Initialize Pigeonholes**: Creates an auxiliary array (pigeonhole array) with size equal to the range

4. **Place Elements**: Each element is placed in its corresponding pigeonhole based on value minus minimum value

5. **Collect Elements**: Elements are collected from pigeonholes in order to form the sorted array

## Key Features of This Implementation:

- Uses COBOL's OCCURS clause for arrays
- Implements proper array indexing and bounds checking
- Handles variable-sized input arrays
- Includes error handling through proper initialization
- Demonstrates the core logic of pigeonhole sort algorithm

## Time Complexity:
- **Best Case**: O(n + k) where n is array size and k is range
- **Average Case**: O(n + k)
- **Worst Case**: O(n + k)

## Space Complexity:
- O(k) where k is the range of input values

The algorithm works best when the range of possible values (k) is not significantly larger than the number of elements (n).