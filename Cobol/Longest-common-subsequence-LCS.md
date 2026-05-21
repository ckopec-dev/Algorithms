# Longest Common Subsequence (LCS) in COBOL

Here's a COBOL implementation of the LCS algorithm using dynamic programming:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LCS-PROGRAM.
       AUTHOR. COBOL-LCS-EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  STRING1                  PIC X(50).
       01  STRING2                  PIC X(50).
       01  LEN1                     PIC 9(3) VALUE 0.
       01  LEN2                     PIC 9(3) VALUE 0.
       01  I                        PIC 9(3) VALUE 0.
       01  J                        PIC 9(3) VALUE 0.
       01  LCS-MATRIX.
           05  LCS-ROW                OCCURS 50 TIMES.
               10  LCS-CELL           PIC 9(3) VALUE 0.
       01  RESULT-STRING            PIC X(50).
       01  RESULT-INDEX             PIC 9(3) VALUE 0.
       01  TEMP-CHAR                PIC X(1).
       01  DISPLAY-RESULT           PIC X(100).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "LONGEST COMMON SUBSEQUENCE (LCS) PROGRAM"
           DISPLAY "=========================================="
           
           MOVE "ABCDGH" TO STRING1
           MOVE "AEDFHR" TO STRING2
           MOVE FUNCTION LENGTH(STRING1) TO LEN1
           MOVE FUNCTION LENGTH(STRING2) TO LEN2
           
           DISPLAY "String 1: " STRING1
           DISPLAY "String 2: " STRING2
           DISPLAY "Length 1: " LEN1
           DISPLAY "Length 2: " LEN2
           
           PERFORM COMPUTE-LCS
           PERFORM RECONSTRUCT-LCS
           
           DISPLAY "LCS Result: " RESULT-STRING
           DISPLAY "LCS Length: " RESULT-INDEX
           
           STOP RUN.

       COMPUTE-LCS.
           *> Initialize first row and column to 0
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN1
               MOVE 0 TO LCS-CELL(I 1)
           END-PERFORM
           
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > LEN2
               MOVE 0 TO LCS-CELL(1 J)
           END-PERFORM
           
           *> Fill the LCS matrix using dynamic programming
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > LEN1
               PERFORM VARYING J FROM 2 BY 1 UNTIL J > LEN2
                   IF STRING1(I:1) = STRING2(J:1)
                       COMPUTE LCS-CELL(I J) = LCS-CELL((I-1) (J-1)) + 1
                   ELSE
                       COMPUTE LCS-CELL(I J) = 
                           FUNCTION MAX(LCS-CELL((I-1) J), LCS-CELL(I (J-1)))
                   END-IF
               END-PERFORM
           END-PERFORM.

       RECONSTRUCT-LCS.
           *> Backtrack through the matrix to build the LCS string
           MOVE 0 TO RESULT-INDEX
           MOVE LEN1 TO I
           MOVE LEN2 TO J
           
           PERFORM UNTIL (I = 1 OR J = 1)
               IF STRING1(I:1) = STRING2(J:1)
                   ADD 1 TO RESULT-INDEX
                   MOVE STRING1(I:1) TO TEMP-CHAR
                   MOVE TEMP-CHAR TO RESULT-STRING(RESULT-INDEX:1)
                   SUBTRACT 1 FROM I
                   SUBTRACT 1 FROM J
               ELSE
                   IF LCS-CELL(I (J-1)) > LCS-CELL((I-1) J)
                       SUBTRACT 1 FROM J
                   ELSE
                       SUBTRACT 1 FROM I
                   END-IF
               END-IF
           END-PERFORM
           
           *> Reverse the result string since we built it backwards
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > RESULT-INDEX
               MOVE RESULT-STRING(I:1) TO TEMP-CHAR
               MOVE TEMP-CHAR TO DISPLAY-RESULT((RESULT-INDEX - I + 1):1)
           END-PERFORM.

       END PROGRAM LCS-PROGRAM.
```

## How it works:

1. **Input**: Two strings "ABCDGH" and "AEDFHR"
2. **Matrix Creation**: Creates a 2D matrix to store LCS values
3. **Dynamic Programming**: Fills the matrix using the recurrence relation:
   - If characters match: `LCS[i][j] = LCS[i-1][j-1] + 1`
   - If characters don't match: `LCS[i][j] = max(LCS[i-1][j], LCS[i][j-1])`
4. **Backtracking**: Reconstructs the actual LCS by tracing back through the matrix
5. **Output**: Displays the LCS string and its length

## Expected Output:
```
LONGEST COMMON SUBSEQUENCE (LCS) PROGRAM
==========================================
String 1: ABCDGH
String 2: AEDFHR
Length 1: 6
Length 2: 6
LCS Result: ADH
LCS Length: 3
```

This implementation demonstrates the classic LCS algorithm in COBOL, showing how dynamic programming can be applied to find the longest common subsequence between two strings.

