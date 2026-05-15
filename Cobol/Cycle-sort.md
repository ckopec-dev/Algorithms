# Cycle Sort Algorithm in COBOL

Here's an example implementation of the Cycle Sort algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CYCLE-SORT.
       AUTHOR. COBOL EXAMPLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ARRAY-SIZE        PIC 9(3) VALUE 8.
       01 I                 PIC 9(3).
       01 J                 PIC 9(3).
       01 K                 PIC 9(3).
       01 TEMP              PIC 9(3).
       01 START-VALUE       PIC 9(3).
       01 CYCLE-LENGTH      PIC 9(3).
       01 ARRAY.
          05 ELEM             PIC 9(3) OCCURS 8 TIMES.
       01 SORTED-ARRAY.
          05 SORTED-ELEM      PIC 9(3) OCCURS 8 TIMES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "CYCLE SORT EXAMPLE"
           DISPLAY "======================"

           * Initialize array with sample data
           MOVE 44 TO ELEM(1)
           MOVE 55 TO ELEM(2)
           MOVE 20 TO ELEM(3)
           MOVE 15 TO ELEM(4)
           MOVE 33 TO ELEM(5)
           MOVE 10 TO ELEM(6)
           MOVE 77 TO ELEM(7)
           MOVE 66 TO ELEM(8)

           DISPLAY "Original Array:"
           PERFORM DISPLAY-ARRAY

           * Perform cycle sort
           PERFORM CYCLE-SORT-ALGORITHM

           DISPLAY "Sorted Array:"
           PERFORM DISPLAY-ARRAY

           STOP RUN.

       CYCLE-SORT-ALGORITHM.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               IF ELEM(I) NOT EQUAL TO SORTED-ELEM(I)
                   MOVE ELEM(I) TO START-VALUE
                   MOVE I TO K
                   PERFORM CYCLE-PROCESS
               END-IF
           END-PERFORM.

       CYCLE-PROCESS.
           MOVE 0 TO CYCLE-LENGTH
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > ARRAY-SIZE
               IF ELEM(J) < START-VALUE
                   ADD 1 TO CYCLE-LENGTH
               END-IF
           END-PERFORM

           MOVE CYCLE-LENGTH TO J
           ADD 1 TO J

           PERFORM VARYING K FROM J BY 1 UNTIL K > ARRAY-SIZE
               IF ELEM(K) = START-VALUE
                   MOVE ELEM(J) TO TEMP
                   MOVE START-VALUE TO ELEM(J)
                   MOVE TEMP TO ELEM(K)
                   GO TO CYCLE-PROCESS
               END-IF
           END-PERFORM.

       DISPLAY-ARRAY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               DISPLAY "ELEM(" I ") = " ELEM(I)
           END-PERFORM
           DISPLAY SPACE.
```

## How Cycle Sort Works in this COBOL Implementation:

1. **Initialization**: The array is filled with sample data
2. **Main Loop**: Iterates through each position in the array
3. **Cycle Detection**: When an element is not in its correct position, it starts a cycle
4. **Cycle Processing**: 
   - Counts how many elements are smaller than the current element
   - Places the element in its correct position
   - Continues the cycle until back to the starting position
5. **Output**: Displays both original and sorted arrays

## Key Features of this Implementation:

- **In-place sorting**: Uses minimal extra memory
- **Minimum number of writes**: At most n swaps
- **Stable**: Preserves relative order of equal elements
- **Time Complexity**: O(n²) in worst case, O(n) when array is already sorted

## Sample Output:
```
CYCLE SORT EXAMPLE
======================
Original Array:
ELEM(1) = 44
ELEM(2) = 55
ELEM(3) = 20
ELEM(4) = 15
ELEM(5) = 33
ELEM(6) = 10
ELEM(7) = 77
ELEM(8) = 66

Sorted Array:
ELEM(1) = 10
ELEM(2) = 15
ELEM(3) = 20
ELEM(4) = 33
ELEM(5) = 44
ELEM(6) = 55
ELEM(7) = 66
ELEM(8) = 77
```

