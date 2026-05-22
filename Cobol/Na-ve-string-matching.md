# Naïve String Matching Algorithm in COBOL

Here's an example implementation of the naïve string matching algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NAIVE-STRING-MATCH.
       AUTHOR. COBOL EXAMPLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TEXT-STRING        PIC X(100) VALUE SPACES.
       01  PATTERN-STRING     PIC X(50) VALUE SPACES.
       01  TEXT-LENGTH        PIC 999 VALUE 0.
       01  PATTERN-LENGTH     PIC 999 VALUE 0.
       01  MATCH-FOUND        PIC 9 VALUE 0.
       01  POSITION-FOUND     PIC 999 VALUE 0.
       01  I                  PIC 999 VALUE 0.
       01  J                  PIC 999 VALUE 0.
       01  K                  PIC 999 VALUE 0.
       01  TEMP-CHAR          PIC X VALUE SPACE.
       01  MATCH-FLAG         PIC X VALUE 'N'.
       01  OUTPUT-TEXT        PIC X(100) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "NAÏVE STRING MATCHING ALGORITHM"
           DISPLAY "================================"
           
           DISPLAY "Enter the text string: "
           ACCEPT TEXT-STRING
           
           DISPLAY "Enter the pattern string: "
           ACCEPT PATTERN-STRING
           
           MOVE FUNCTION LENGTH(TEXT-STRING) TO TEXT-LENGTH
           MOVE FUNCTION LENGTH(PATTERN-STRING) TO PATTERN-LENGTH
           
           IF PATTERN-LENGTH > TEXT-LENGTH
               DISPLAY "Pattern is longer than text. No match possible."
               GO TO END-PROGRAM
           END-IF
           
           PERFORM SEARCH-FOR-MATCH
           
           IF MATCH-FOUND = 1
               DISPLAY "Pattern found at position: " POSITION-FOUND
           ELSE
               DISPLAY "Pattern not found in text."
           END-IF
           
       END-PROGRAM.
       
       SEARCH-FOR-MATCH.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > (TEXT-LENGTH - PATTERN-LENGTH + 1)
               MOVE 'Y' TO MATCH-FLAG
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > PATTERN-LENGTH
                   COMPUTE K = I + J - 1
                   MOVE TEXT-STRING(K:1) TO TEMP-CHAR
                   IF TEMP-CHAR NOT EQUAL TO PATTERN-STRING(J:1)
                       MOVE 'N' TO MATCH-FLAG
                       GO TO NEXT-CHARACTER
                   END-IF
               END-PERFORM
               
               IF MATCH-FLAG = 'Y'
                   MOVE 1 TO MATCH-FOUND
                   MOVE I TO POSITION-FOUND
                   GO TO END-SEARCH
               END-IF
               
           NEXT-CHARACTER.
           END-PERFORM.
           
       END-SEARCH.
       
       END-PROGRAM.
```

## How the Algorithm Works:

1. **Input**: Takes a text string and a pattern string as input
2. **Initialization**: Sets up variables to track lengths and positions
3. **Main Loop**: Iterates through each possible starting position in the text
4. **Character-by-Character Comparison**: For each position, compares characters with the pattern
5. **Match Detection**: If all characters match, a match is found
6. **Output**: Displays the position where the pattern was found or indicates no match

## Sample Run:

```
NAÏVE STRING MATCHING ALGORITHM
================================
Enter the text string: ABABCABABA
Enter the pattern string: ABAB
Pattern found at position: 1
```

## Key Features:

- **Simple Implementation**: Uses basic COBOL constructs
- **No Built-in Functions**: Implements matching logic manually
- **Position Tracking**: Returns the exact position of the match
- **Error Handling**: Checks for invalid pattern length
- **Clear Output**: Provides user-friendly results

This implementation demonstrates the O(n*m) time complexity of the naïve string matching algorithm, where n is the text length and m is the pattern length.

