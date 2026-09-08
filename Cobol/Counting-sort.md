# Counting Sort Algorithm in COBOL

Here's an example implementation of the Counting Sort algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUNTING-SORT.
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
       FD  INPUT-FILE.
       01  INPUT-RECORD.
           05  INPUT-NUMBER    PIC 9(3).

       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD.
           05  OUTPUT-NUMBER   PIC 9(3).

       WORKING-STORAGE SECTION.
       01  MAX-VALUE           PIC 9(3) VALUE 100.
       01  COUNT-SIZE          PIC 9(3) VALUE 101.
       01  I                   PIC 9(3).
       01  J                   PIC 9(3).
       01  TEMP-NUMBER         PIC 9(3).
       01  SORT-ARRAY.
           05  SORT-ELEMENT    OCCURS 100 TIMES PIC 9(3).
       01  COUNT-ARRAY.
           05  COUNT-ELEMENT   OCCURS 101 TIMES PIC 9(5) VALUE 0.
       01  SORTED-ARRAY.
           05  SORTED-ELEMENT  OCCURS 100 TIMES PIC 9(3).
       01  RECORD-COUNT        PIC 9(3) VALUE 0.
       01  EOF-FLAG            PIC X VALUE 'N'.
           88  END-OF-FILE     VALUE 'Y'.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM INITIALIZE-VARIABLES.
           PERFORM READ-INPUT-DATA.
           PERFORM COUNTING-SORT-ALGORITHM.
           PERFORM WRITE-SORTED-DATA.
           STOP RUN.

       INITIALIZE-VARIABLES.
           MOVE ZERO TO RECORD-COUNT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 101
               MOVE ZERO TO COUNT-ELEMENT(I)
           END-PERFORM.

       READ-INPUT-DATA.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE INTO INPUT-NUMBER
               AT END MOVE 'Y' TO EOF-FLAG
           END-READ.
           PERFORM UNTIL END-OF-FILE
               ADD 1 TO RECORD-COUNT
               MOVE INPUT-NUMBER TO SORT-ELEMENT(RECORD-COUNT)
               READ INPUT-FILE INTO INPUT-NUMBER
                   AT END MOVE 'Y' TO EOF-FLAG
               END-READ
           END-PERFORM.
           CLOSE INPUT-FILE.

       COUNTING-SORT-ALGORITHM.
           PERFORM COUNT-FREQUENCIES.
           PERFORM RECONSTRUCT-SORTED-ARRAY.

       COUNT-FREQUENCIES.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > RECORD-COUNT
               ADD 1 TO COUNT-ELEMENT(SORT-ELEMENT(I))
           END-PERFORM.

       RECONSTRUCT-SORTED-ARRAY.
           MOVE 0 TO J.
           PERFORM VARYING I FROM 0 BY 1 UNTIL I > MAX-VALUE
               PERFORM VARYING TEMP-NUMBER FROM 1 BY 1
                   UNTIL TEMP-NUMBER > COUNT-ELEMENT(I) OR J > RECORD-COUNT
                   ADD 1 TO J
                   MOVE I TO SORTED-ELEMENT(J)
               END-PERFORM
           END-PERFORM.

       WRITE-SORTED-DATA.
           OPEN OUTPUT OUTPUT-FILE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > RECORD-COUNT
               MOVE SORTED-ELEMENT(I) TO OUTPUT-NUMBER
               WRITE OUTPUT-RECORD
           END-PERFORM.
           CLOSE OUTPUT-FILE.
```

## How it works:

1. **Initialization**: Sets up arrays and variables for counting sort
2. **Input Reading**: Reads numbers from input file into array
3. **Counting Phase**: Counts frequency of each element using count array
4. **Reconstruction Phase**: Rebuilds sorted array based on counts
5. **Output Writing**: Writes sorted numbers to output file

## Key Features:

- Uses counting sort for efficient sorting when range is known and limited
- Handles up to 100 elements with values from 0-100
- Reads from input file and writes to output file
- Demonstrates COBOL array handling and loop constructs

## Sample Input File (INPUT.DAT):
```
45
23
67
23
89
12
45
78
```

## Sample Output File (OUTPUT.DAT):
```
12
23
23
45
45
67
78
89
```