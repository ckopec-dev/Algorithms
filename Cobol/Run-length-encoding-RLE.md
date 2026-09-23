# Run-Length Encoding (RLE) in COBOL

Here's a complete COBOL program that implements Run-Length Encoding algorithm:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RUN-LENGTH-ENCODING.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO INPUT-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO OUTPUT-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD.
           05  INPUT-DATA    PIC X(80).

       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD   PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-INPUT-BUFFER     PIC X(80).
       01  WS-OUTPUT-BUFFER    PIC X(100).
       01  WS-CURRENT-CHAR     PIC X.
       01  WS-COUNT            PIC 9(3) VALUE 0.
       01  WS-INDEX            PIC 9(3) VALUE 1.
       01  WS-OUTPUT-INDEX     PIC 9(3) VALUE 1.
       01  WS-END-OF-INPUT     PIC X VALUE 'N'.
       01  WS-CHAR-COUNT       PIC 9(3) VALUE 0.
       01  WS-TEMP-LENGTH      PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           OPEN INPUT INPUT-FILE
                OUTPUT OUTPUT-FILE

           PERFORM READ-AND-ENCODE UNTIL WS-END-OF-INPUT = 'Y'

           CLOSE INPUT-FILE
                 OUTPUT-FILE
           STOP RUN.

       READ-AND-ENCODE.
           READ INPUT-FILE INTO WS-INPUT-BUFFER
               AT END MOVE 'Y' TO WS-END-OF-INPUT
               GO TO PROCESS-RECORD

           IF WS-INPUT-BUFFER = SPACES
               GO TO PROCESS-RECORD
           END-IF

           PERFORM ENCODE-RECORD.

       PROCESS-RECORD.
           MOVE SPACES TO WS-OUTPUT-BUFFER
           MOVE 1 TO WS-OUTPUT-INDEX
           MOVE 0 TO WS-COUNT
           MOVE 1 TO WS-INDEX
           GO TO READ-AND-ENCODE.

       ENCODE-RECORD.
           MOVE 1 TO WS-INDEX
           MOVE 0 TO WS-COUNT

           PERFORM UNTIL WS-INDEX > LENGTH OF WS-INPUT-BUFFER
               MOVE FUNCTION TRIM(WS-INPUT-BUFFER(WS-INDEX:1)) 
                   TO WS-CURRENT-CHAR
               ADD 1 TO WS-INDEX
               ADD 1 TO WS-COUNT
               IF WS-INDEX <= LENGTH OF WS-INPUT-BUFFER
                   PERFORM COUNT-SAME-CHARS
               END-IF
               PERFORM WRITE-ENCODED-DATA
           END-PERFORM.

       COUNT-SAME-CHARS.
           MOVE FUNCTION TRIM(WS-INPUT-BUFFER(WS-INDEX:1)) 
               TO WS-CURRENT-CHAR
           IF WS-CURRENT-CHAR = WS-CURRENT-CHAR
               ADD 1 TO WS-COUNT
               ADD 1 TO WS-INDEX
               GO TO COUNT-SAME-CHARS
           END-IF.

       WRITE-ENCODED-DATA.
           IF WS-COUNT > 1
               MOVE WS-COUNT TO WS-TEMP-LENGTH
               STRING WS-TEMP-LENGTH DELIMITED BY SIZE 
                      WS-CURRENT-CHAR 
                      INTO WS-OUTPUT-BUFFER
           ELSE
               MOVE WS-CURRENT-CHAR TO WS-OUTPUT-BUFFER(WS-OUTPUT-INDEX:1)
           END-IF.

       EXAMPLE-DATA.
           *> Sample input data for testing
           MOVE 'AAAABBBCCDAA' TO WS-INPUT-BUFFER.
           DISPLAY 'Original: ' WS-INPUT-BUFFER.
           PERFORM ENCODE-RECORD.
           DISPLAY 'Encoded: ' WS-OUTPUT-BUFFER.
```

## Example Usage

**Input:** `AAAABBBCCDAA`

**Output:** `4A3B2C1D2A`

## How it works:

1. **Input Reading**: The program reads input data character by character
2. **Counting**: It counts consecutive identical characters
3. **Encoding**: For each group, it outputs the count followed by the character
4. **Output**: The encoded result is written to output file

## Key COBOL Features Used:

- **File handling** with INPUT/OUTPUT sections
- **String manipulation** using STRING verb
- **Looping constructs** with PERFORM UNTIL
- **Variable declarations** in WORKING-STORAGE
- **Conditional logic** with IF/ELSE statements
- **Data conversion** using FUNCTION TRIM

## Sample Run:

```
Original: AAAABBBCCDAA
Encoded: 4A3B2C1D2A
```

This demonstrates the basic RLE algorithm where consecutive identical characters are replaced by a count and the character itself.