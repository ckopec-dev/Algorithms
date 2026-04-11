# Merge Sort Algorithm in COBOL

Here's an example implementation of the merge sort algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MERGE-SORT.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "INPUT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "OUTPUT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
           05 RECORD-NUMBER PIC 9(5).
           05 RECORD-VALUE  PIC 9(5).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
           05 OUTPUT-NUMBER PIC 9(5).
           05 OUTPUT-VALUE  PIC 9(5).

       WORKING-STORAGE SECTION.
       01 SORT-ARRAY.
           05 ARRAY-ELEMENT OCCURS 100 TIMES PIC 9(5).
       01 TEMP-ARRAY.
           05 TEMP-ELEMENT OCCURS 100 TIMES PIC 9(5).
       01 ARRAY-SIZE PIC 9(3) VALUE 0.
       01 I PIC 9(3) VALUE 0.
       01 J PIC 9(3) VALUE 0.
       01 K PIC 9(3) VALUE 0.
       01 L PIC 9(3) VALUE 0.
       01 R PIC 9(3) VALUE 0.
       01 M PIC 9(3) VALUE 0.
       01 TEMP-VALUE PIC 9(5) VALUE 0.
       01 EOF-FLAG PIC X VALUE 'N'.
           88 END-OF-FILE VALUE 'Y'.
       01 SORT-FLAG PIC X VALUE 'N'.
           88 SORTED VALUE 'Y'.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-ARRAY
           PERFORM READ-DATA
           PERFORM MERGE-SORT-RECURSIVE
           PERFORM WRITE-RESULTS
           STOP RUN.

       INITIALIZE-ARRAY.
           MOVE 0 TO ARRAY-SIZE
           MOVE 0 TO I
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 100
               MOVE 0 TO ARRAY-ELEMENT(I)
           END-PERFORM.

       READ-DATA.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE
               AT END MOVE 'Y' TO EOF-FLAG
           END-READ
           PERFORM UNTIL END-OF-FILE
               ADD 1 TO ARRAY-SIZE
               MOVE RECORD-VALUE TO ARRAY-ELEMENT(ARRAY-SIZE)
               READ INPUT-FILE
                   AT END MOVE 'Y' TO EOF-FLAG
               END-READ
           END-PERFORM
           CLOSE INPUT-FILE.

       MERGE-SORT-RECURSIVE.
           IF ARRAY-SIZE > 1
               PERFORM SPLIT-ARRAY
               PERFORM MERGE-SORT-RECURSIVE
           END-IF.

       SPLIT-ARRAY.
           COMPUTE M = ARRAY-SIZE / 2
           PERFORM SPLIT-LEFT
           PERFORM SPLIT-RIGHT
           PERFORM MERGE-ARRAYS.

       SPLIT-LEFT.
           MOVE 0 TO I
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > M
               MOVE ARRAY-ELEMENT(I) TO TEMP-ARRAY(I)
           END-PERFORM.

       SPLIT-RIGHT.
           MOVE 0 TO I
           MOVE 0 TO J
           PERFORM VARYING I FROM (M + 1) BY 1 UNTIL I > ARRAY-SIZE
               ADD 1 TO J
               MOVE ARRAY-ELEMENT(I) TO TEMP-ARRAY(J)
           END-PERFORM.

       MERGE-ARRAYS.
           MOVE 0 TO I
           MOVE 0 TO J
           MOVE 0 TO K
           MOVE 0 TO L
           MOVE 0 TO R
           COMPUTE L = M
           COMPUTE R = ARRAY-SIZE - M

           PERFORM UNTIL (I > L) OR (J > R)
               IF TEMP-ARRAY(I) <= TEMP-ARRAY(J + L)
                   MOVE TEMP-ARRAY(I) TO ARRAY-ELEMENT(K + 1)
                   ADD 1 TO I
               ELSE
                   MOVE TEMP-ARRAY(J + L) TO ARRAY-ELEMENT(K + 1)
                   ADD 1 TO J
               END-IF
               ADD 1 TO K
           END-PERFORM

           PERFORM UNTIL I > L
               MOVE TEMP-ARRAY(I) TO ARRAY-ELEMENT(K + 1)
               ADD 1 TO I
               ADD 1 TO K
           END-PERFORM

           PERFORM UNTIL J > R
               MOVE TEMP-ARRAY(J + L) TO ARRAY-ELEMENT(K + 1)
               ADD 1 TO J
               ADD 1 TO K
           END-PERFORM.

       WRITE-RESULTS.
           OPEN OUTPUT OUTPUT-FILE
           MOVE 0 TO I
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
               MOVE ARRAY-ELEMENT(I) TO OUTPUT-VALUE
               WRITE OUTPUT-RECORD
           END-PERFORM
           CLOSE OUTPUT-FILE.
```

## Key Features of this Implementation:

1. **Array-based sorting**: Uses working storage arrays to store data
2. **Recursive approach**: Implements the classic divide-and-conquer merge sort
3. **File I/O**: Reads from input file and writes to output file
4. **Proper merging**: Combines two sorted subarrays into one sorted array
5. **Memory management**: Uses temporary arrays for splitting and merging

## How it works:

1. **Initialization**: Sets up arrays and file handling
2. **Data reading**: Reads input data into the main array
3. **Recursive sorting**: Divides the array into halves and recursively sorts them
4. **Merging**: Combines sorted halves back together
5. **Output**: Writes sorted data to output file

This implementation demonstrates the classic merge sort algorithm with O(n log n) time complexity suitable for sorting large datasets in COBOL.

