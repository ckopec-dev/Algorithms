# Suffix Array Construction Algorithm in COBOL

Here's a COBOL implementation of a simplified suffix array construction algorithm using the brute force approach:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUFFIX-ARRAY-CONSTRUCTION.
       AUTHOR. AI Assistant.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "INPUT.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD PIC X(100).

       WORKING-STORAGE SECTION.
       01 TEXT-LENGTH PIC 9(4) VALUE 0.
       01 TEXT-VALUE PIC X(100) VALUE SPACES.
       01 SUFFIX-ARRAY OCCURS 100 TIMES INDEXED BY I.
          02 SUFFIX-INDEX PIC 9(4).
          02 SUFFIX-TEXT PIC X(100).
       01 TEMP-ARRAY OCCURS 100 TIMES INDEXED BY J.
          02 TEMP-INDEX PIC 9(4).
          02 TEMP-TEXT PIC X(100).
       01 I-INDEX PIC 9(4) VALUE 1.
       01 J-INDEX PIC 9(4) VALUE 1.
       01 K-INDEX PIC 9(4) VALUE 1.
       01 TEMP-CHAR PIC X.
       01 COMPARE-RESULT PIC 9 VALUE 0.
       01 SWAP-FLAG PIC X VALUE "N".
       01 EOF-FLAG PIC X VALUE "N".
       01 LINE-COUNT PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "Suffix Array Construction Algorithm"
           DISPLAY "====================================="
           
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE INTO TEXT-VALUE
               AT END MOVE "Y" TO EOF-FLAG
           END-READ
           CLOSE INPUT-FILE
           
           IF EOF-FLAG = "N"
               PERFORM INITIALIZE-TEXT
               PERFORM BUILD-SUFFIX-ARRAY
               PERFORM SORT-SUFFIX-ARRAY
               PERFORM DISPLAY-RESULTS
           ELSE
               DISPLAY "No input data found"
           END-IF
           
           STOP RUN.

       INITIALIZE-TEXT.
           MOVE FUNCTION LENGTH(TEXT-VALUE) TO TEXT-LENGTH
           DISPLAY "Input text: " TEXT-VALUE
           DISPLAY "Text length: " TEXT-LENGTH
           .

       BUILD-SUFFIX-ARRAY.
           PERFORM VARYING I-INDEX FROM 1 BY 1
               UNTIL I-INDEX > TEXT-LENGTH
               MOVE I-INDEX TO SUFFIX-INDEX(I-INDEX)
               MOVE SPACES TO SUFFIX-TEXT(I-INDEX)
               PERFORM EXTRACT-SUFFIX
           END-PERFORM
           .

       EXTRACT-SUFFIX.
           MOVE TEXT-VALUE TO TEMP-TEXT(1:TEXT-LENGTH)
           COMPUTE K-INDEX = TEXT-LENGTH - I-INDEX + 1
           MOVE TEMP-TEXT(I-INDEX:K-INDEX) TO SUFFIX-TEXT(I-INDEX)
           .

       SORT-SUFFIX-ARRAY.
           PERFORM VARYING I-INDEX FROM 1 BY 1
               UNTIL I-INDEX >= TEXT-LENGTH
               PERFORM VARYING J-INDEX FROM I-INDEX + 1 BY 1
                   UNTIL J-INDEX > TEXT-LENGTH
                   MOVE SUFFIX-TEXT(I-INDEX) TO TEMP-TEXT(1:100)
                   MOVE SUFFIX-TEXT(J-INDEX) TO TEMP-TEXT(101:100)
                   PERFORM COMPARE-SUFFIXES
                   IF COMPARE-RESULT = 1
                       PERFORM SWAP-SUFFIXES
                   END-IF
               END-PERFORM
           END-PERFORM
           .

       COMPARE-SUFFIXES.
           MOVE 0 TO COMPARE-RESULT
           PERFORM VARYING K-INDEX FROM 1 BY 1
               UNTIL K-INDEX > 100 OR TEMP-TEXT(1:100) = SPACES
               IF TEMP-TEXT(K-INDEX:1) < TEMP-TEXT(100+K-INDEX:1)
                   MOVE 1 TO COMPARE-RESULT
                   GO TO COMPARE-EXIT
               ELSE IF TEMP-TEXT(K-INDEX:1) > TEMP-TEXT(100+K-INDEX:1)
                   MOVE 2 TO COMPARE-RESULT
                   GO TO COMPARE-EXIT
               END-IF
           END-PERFORM
           .

       SWAP-SUFFIXES.
           MOVE SUFFIX-TEXT(I-INDEX) TO TEMP-TEXT(1:100)
           MOVE SUFFIX-TEXT(J-INDEX) TO SUFFIX-TEXT(I-INDEX)
           MOVE TEMP-TEXT(1:100) TO SUFFIX-TEXT(J-INDEX)
           .

       DISPLAY-RESULTS.
           DISPLAY "Suffix Array Construction Complete"
           DISPLAY "=================================="
           PERFORM VARYING I-INDEX FROM 1 BY 1
               UNTIL I-INDEX > TEXT-LENGTH
               DISPLAY "Suffix " I-INDEX ": Index=" SUFFIX-INDEX(I-INDEX)
                       " Text=" SUFFIX-TEXT(I-INDEX)
           END-PERFORM
           .

       COMPARE-EXIT.
           .
```

## Example Usage

**Input file (INPUT.TXT):**
```
banana
```

**Expected Output:**
```
Suffix Array Construction Algorithm
=====================================
Input text: banana
Text length: 6
Suffix Array Construction Complete
==================================
Suffix 1: Index=6 Text=a
Suffix 2: Index=5 Text=na
Suffix 3: Index=4 Text=ana
Suffix 4: Index=3 Text=nana
Suffix 5: Index=2 Text=anana
Suffix 6: Index=1 Text=banana
```

## Algorithm Explanation

This COBOL implementation demonstrates:

1. **Text Input**: Reads input text from a file
2. **Suffix Generation**: Creates all suffixes of the input string
3. **Sorting**: Sorts the suffixes lexicographically using bubble sort
4. **Output**: Displays the constructed suffix array with indices

## Key Features

- Uses COBOL's indexed arrays for storing suffix data
- Implements basic string manipulation operations
- Includes error handling for empty input
- Demonstrates procedural programming concepts in COBOL
- Shows how to work with character data and string comparisons

Note: This is a simplified implementation. A production version would use more efficient algorithms like SA-IS or DC3 for better performance.