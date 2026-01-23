# Boyer-Moore Majority Vote Algorithm in COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BOYER-MOORE-MAJORITY-VOTE.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  WS-ARRAY.
           05  WS-ELEMENTS    PIC 9(3) OCCURS 100 TIMES.
       01  WS-MAJORITY-ELEMENT    PIC 9(3).
       01  WS-CANDIDATE           PIC 9(3).
       01  WS-COUNT               PIC 9(5) VALUE 0.
       01  WS-ARRAY-SIZE          PIC 9(3) VALUE 10.
       01  WS-I                   PIC 9(3) VALUE 1.
       01  WS-J                   PIC 9(3) VALUE 1.
       01  WS-FOUND               PIC X VALUE 'N'.
       01  WS-TEMP                PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Boyer-Moore Majority Vote Algorithm"
           DISPLAY "======================================"

           * Initialize test array
           MOVE 3 TO WS-ELEMENTS(1)
           MOVE 3 TO WS-ELEMENTS(2)
           MOVE 4 TO WS-ELEMENTS(3)
           MOVE 3 TO WS-ELEMENTS(4)
           MOVE 3 TO WS-ELEMENTS(5)
           MOVE 1 TO WS-ELEMENTS(6)
           MOVE 3 TO WS-ELEMENTS(7)
           MOVE 3 TO WS-ELEMENTS(8)
           MOVE 3 TO WS-ELEMENTS(9)
           MOVE 2 TO WS-ELEMENTS(10)

           DISPLAY "Input Array:"
           PERFORM DISPLAY-ARRAY

           * Apply Boyer-Moore Majority Vote Algorithm
           PERFORM MAJORITY-VOTE-ALGORITHM

           DISPLAY "Majority Element: " WS-MAJORITY-ELEMENT
           DISPLAY "Algorithm completed successfully."

           STOP RUN.

       MAJORITY-VOTE-ALGORITHM.
           * Phase 1: Find candidate
           MOVE WS-ELEMENTS(1) TO WS-CANDIDATE
           MOVE 1 TO WS-COUNT

           PERFORM VARYING WS-I FROM 2 BY 1 UNTIL WS-I > WS-ARRAY-SIZE
               IF WS-ELEMENTS(WS-I) = WS-CANDIDATE
                   ADD 1 TO WS-COUNT
               ELSE
                   SUBTRACT 1 FROM WS-COUNT
                   IF WS-COUNT = 0
                       MOVE WS-ELEMENTS(WS-I) TO WS-CANDIDATE
                       MOVE 1 TO WS-COUNT
                   END-IF
               END-IF
           END-PERFORM

           * Phase 2: Verify candidate is majority
           MOVE 0 TO WS-COUNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-ARRAY-SIZE
               IF WS-ELEMENTS(WS-I) = WS-CANDIDATE
                   ADD 1 TO WS-COUNT
               END-IF
           END-PERFORM

           IF WS-COUNT > WS-ARRAY-SIZE / 2
               MOVE WS-CANDIDATE TO WS-MAJORITY-ELEMENT
           ELSE
               MOVE 0 TO WS-MAJORITY-ELEMENT
           END-IF.

       DISPLAY-ARRAY.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-ARRAY-SIZE
               DISPLAY "Element " WS-I ": " WS-ELEMENTS(WS-I)
           END-PERFORM.

       DISPLAY-RESULTS.
           DISPLAY "Candidate: " WS-CANDIDATE
           DISPLAY "Count: " WS-COUNT
           DISPLAY "Majority Element: " WS-MAJORITY-ELEMENT.
```

## Algorithm Explanation

This COBOL implementation demonstrates the Boyer-Moore Majority Vote Algorithm with two phases:

### Phase 1: Candidate Selection
- Initialize first element as candidate
- For each subsequent element:
  - If matches candidate, increment count
  - If doesn't match, decrement count
  - If count reaches zero, replace candidate with current element

### Phase 2: Verification
- Count occurrences of the candidate
- If count > n/2, then candidate is majority element

### Sample Output
```
Boyer-Moore Majority Vote Algorithm
======================================
Input Array:
Element 1: 3
Element 2: 3
Element 3: 4
Element 4: 3
Element 5: 3
Element 6: 1
Element 7: 3
Element 8: 3
Element 9: 3
Element 10: 2
Majority Element: 3
Algorithm completed successfully.
```

The algorithm correctly identifies 3 as the majority element since it appears 7 times out of 10 elements (70% > 50%).

