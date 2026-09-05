# BogoSort Algorithm in COBOL

Here's an implementation of the BogoSort (also known as "Stupid Sort") algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BogoSort.
       AUTHOR. AI Assistant.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ARRAY-SIZE        PIC 9(3) VALUE 5.
       01  ARRAY-TABLE.
           05  ARR-ITEM       OCCURS 100 TIMES PIC 9(3).
       01  TEMP-ITEM         PIC 9(3).
       01  I-INDEX           PIC 9(3).
       01  J-INDEX           PIC 9(3).
       01  SWAP-FLAG         PIC X VALUE 'N'.
       01  SORTED-FLAG       PIC X VALUE 'N'.
       01  RANDOM-NUMBER     PIC 9(3).
       01  TOTAL-ATTEMPTS    PIC 9(6) VALUE 0.
       01  DISPLAY-TEXT      PIC X(50).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "BogoSort Algorithm Demo"
           DISPLAY "========================="
           
           *> Initialize array with sample data
           MOVE 42 TO ARR-ITEM (1)
           MOVE 15 TO ARR-ITEM (2)
           MOVE 87 TO ARR-ITEM (3)
           MOVE 33 TO ARR-ITEM (4)
           MOVE 69 TO ARR-ITEM (5)
           
           DISPLAY "Original Array:"
           PERFORM DISPLAY-ARRAY
           
           *> Perform BogoSort
           PERFORM Bogo-Sort-Logic
           
           DISPLAY "Sorted Array:"
           PERFORM DISPLAY-ARRAY
           DISPLAY "Total attempts: " TOTAL-ATTEMPTS
           
           STOP RUN.

       Bogo-Sort-Logic.
           MOVE 'N' TO SORTED-FLAG
           PERFORM UNTIL SORTED-FLAG = 'Y'
               ADD 1 TO TOTAL-ATTEMPTS
               PERFORM RANDOMIZE-ARRAY
               IF PERFORM IS-SORTED
                   MOVE 'Y' TO SORTED-FLAG
               END-IF
           END-PERFORM.

       RANDOMIZE-ARRAY.
           *> Fisher-Yates shuffle algorithm
           PERFORM VARYING I-INDEX FROM ARRAY-SIZE BY -1 
               UNTIL I-INDEX < 2
               COMPUTE RANDOM-NUMBER = FUNCTION RANDOM * I-INDEX + 1
               IF RANDOM-NUMBER NOT = I-INDEX
                   MOVE ARR-ITEM (I-INDEX) TO TEMP-ITEM
                   MOVE ARR-ITEM (RANDOM-NUMBER) TO ARR-ITEM (I-INDEX)
                   MOVE TEMP-ITEM TO ARR-ITEM (RANDOM-NUMBER)
               END-IF
           END-PERFORM.

       IS-SORTED.
           MOVE 'Y' TO SORTED-FLAG
           PERFORM VARYING I-INDEX FROM 1 BY 1 
               UNTIL I-INDEX >= ARRAY-SIZE
               IF ARR-ITEM (I-INDEX) > ARR-ITEM (I-INDEX + 1)
                   MOVE 'N' TO SORTED-FLAG
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM.

       DISPLAY-ARRAY.
           MOVE 0 TO I-INDEX
           PERFORM VARYING I-INDEX FROM 1 BY 1 
               UNTIL I-INDEX > ARRAY-SIZE
               STRING 
                   "Item " I-INDEX ": " ARR-ITEM (I-INDEX) 
                   "   " INTO DISPLAY-TEXT
               DISPLAY DISPLAY-TEXT
           END-PERFORM.
```

## How BogoSort Works in COBOL

This implementation demonstrates the BogoSort algorithm with the following key components:

### Key Features:
1. **Array Initialization**: Creates a sample array of 5 elements
2. **Random Shuffle**: Uses Fisher-Yates shuffle to randomize array elements
3. **Sorting Check**: Validates if array is sorted in ascending order
4. **Loop Control**: Continues until array is randomly sorted
5. **Statistics Tracking**: Counts total attempts needed

### Algorithm Steps:
1. Start with an unsorted array
2. Randomly shuffle the elements
3. Check if array is sorted
4. If not sorted, repeat steps 2-3
5. Continue until array happens to be sorted by chance

### Important Notes:
- **Time Complexity**: O((n+1)!) in worst case - extremely inefficient
- **Space Complexity**: O(1) - sorts in place
- **Practical Use**: Only for educational purposes due to exponential time complexity
- **Randomness**: Uses COBOL's FUNCTION RANDOM for shuffling

This implementation shows how BogoSort works conceptually, though in practice it would be extremely slow for any reasonable array size.