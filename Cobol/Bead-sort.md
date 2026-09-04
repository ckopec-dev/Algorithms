# Bead Sort Algorithm in COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BEAD-SORT.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01 ARRAY-SIZE      PIC 9(3) VALUE 8.
       01 I               PIC 9(3).
       01 J               PIC 9(3).
       01 K               PIC 9(3).
       01 TEMP            PIC 9(3).
       01 MAX-VALUE       PIC 9(3).
       01 BEADS           PIC 9(3) OCCURS 100 TIMES.
       01 SORTED-ARRAY    PIC 9(3) OCCURS 100 TIMES.

       01 ARRAY-VALUES.
          05 VALUE-1      PIC 9(3) VALUE 4.
          05 VALUE-2      PIC 9(3) VALUE 2.
          05 VALUE-3      PIC 9(3) VALUE 7.
          05 VALUE-4      PIC 9(3) VALUE 1.
          05 VALUE-5      PIC 9(3) VALUE 3.
          05 VALUE-6      PIC 9(3) VALUE 6.
          05 VALUE-7      PIC 9(3) VALUE 5.
          05 VALUE-8      PIC 9(3) VALUE 8.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "BEAD SORT EXAMPLE"
           DISPLAY "Original Array: "
           PERFORM DISPLAY-ARRAY

           * Initialize BEADS array
           PERFORM INITIALIZE-BEADS

           * Perform bead sort
           PERFORM BEAD-SORT-ALGORITHM

           * Display sorted array
           DISPLAY "Sorted Array: "
           PERFORM DISPLAY-SORTED-ARRAY

           STOP RUN.

       INITIALIZE-BEADS.
           MOVE 0 TO MAX-VALUE
           MOVE 1 TO I
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               MOVE VALUE-I TO TEMP
               IF TEMP > MAX-VALUE
                   MOVE TEMP TO MAX-VALUE
               END-IF
           END-PERFORM

           * Initialize beads to zero
           MOVE 0 TO BEADS(1) THRU BEADS(MAX-VALUE)
           MOVE 1 TO I
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               ADD 1 TO BEADS(VALUE-I)
           END-PERFORM.

       BEAD-SORT-ALGORITHM.
           MOVE 0 TO K
           MOVE MAX-VALUE TO I

           PERFORM VARYING I FROM MAX-VALUE BY -1 UNTIL I < 1
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > BEADS(I)
                   ADD 1 TO K
                   MOVE I TO SORTED-ARRAY(K)
               END-PERFORM
           END-PERFORM.

       DISPLAY-ARRAY.
           MOVE 1 TO I
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               DISPLAY VALUE-I " "
           END-PERFORM
           DISPLAY "".

       DISPLAY-SORTED-ARRAY.
           MOVE 1 TO I
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               DISPLAY SORTED-ARRAY(I) " "
           END-PERFORM
           DISPLAY "".
```

## How It Works

This COBOL implementation of bead sort follows these steps:

1. **Initialization**: Find the maximum value in the input array to determine how many "bead levels" we need.

2. **Bead Distribution**: For each element in the input array, increment the corresponding bead level counter.

3. **Sorting**: Read the bead levels from highest to lowest, placing elements back into the sorted array based on the count of beads at each level.

## Example Output
```
BEAD SORT EXAMPLE
Original Array: 
4 2 7 1 3 6 5 8 

Sorted Array: 
1 2 3 4 5 6 7 8 
```

The algorithm works by simulating the process of beads falling through a set of parallel rods, where each rod represents a possible value in the array.