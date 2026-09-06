# Caesar Cipher in COBOL

Here's an example implementation of the Caesar cipher algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAESAR-CIPHER.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "INPUT.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "OUTPUT.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-LINE PIC X(80).

       FD OUTPUT-FILE.
       01 OUTPUT-LINE PIC X(80).

       WORKING-STORAGE SECTION.
       01 PLAIN-TEXT        PIC X(80).
       01 CIPHER-TEXT       PIC X(80).
       01 SHIFT-VALUE       PIC 99 VALUE 3.
       01 I                 PIC 99 VALUE 1.
       01 J                 PIC 99 VALUE 1.
       01 CHAR-VALUE        PIC 99.
       01 NEW-CHAR          PIC X.
       01 TEMP-CHAR         PIC X.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Caesar Cipher Program"
           DISPLAY "Shift value: " SHIFT-VALUE
           DISPLAY "------------------------"

           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           PERFORM READ-AND-PROCESS UNTIL END-OF-FILE

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE

           STOP RUN.

       READ-AND-PROCESS.
           READ INPUT-FILE INTO PLAIN-TEXT
               AT END MOVE "END" TO PLAIN-TEXT
           END-READ

           IF PLAIN-TEXT IS NOT EQUAL TO "END"
               PERFORM ENCRYPT-TEXT
               WRITE OUTPUT-LINE FROM CIPHER-TEXT
               DISPLAY "Original:  " PLAIN-TEXT
               DISPLAY "Encrypted: " CIPHER-TEXT
               DISPLAY "------------------------"
           END-IF.

       ENCRYPT-TEXT.
           MOVE SPACES TO CIPHER-TEXT
           MOVE 1 TO J

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH OF PLAIN-TEXT
               MOVE PLAIN-TEXT(I:1) TO TEMP-CHAR
               
               IF TEMP-CHAR IS ALPHABETIC
                   COMPUTE CHAR-VALUE = FUNCTION ORD(TEMP-CHAR)
                   
                   IF TEMP-CHAR >= "A" AND TEMP-CHAR <= "Z"
                       COMPUTE CHAR-VALUE = CHAR-VALUE - 65
                       COMPUTE CHAR-VALUE = (CHAR-VALUE + SHIFT-VALUE) MOD 26
                       COMPUTE CHAR-VALUE = CHAR-VALUE + 65
                       MOVE FUNCTION CHR(CHAR-VALUE) TO NEW-CHAR
                   ELSE
                       COMPUTE CHAR-VALUE = CHAR-VALUE - 97
                       COMPUTE CHAR-VALUE = (CHAR-VALUE + SHIFT-VALUE) MOD 26
                       COMPUTE CHAR-VALUE = CHAR-VALUE + 97
                       MOVE FUNCTION CHR(CHAR-VALUE) TO NEW-CHAR
                   END-IF
               ELSE
                   MOVE TEMP-CHAR TO NEW-CHAR
               END-IF

               MOVE NEW-CHAR TO CIPHER-TEXT(J:1)
               ADD 1 TO J
           END-PERFORM.

       DECRYPT-TEXT.
           MOVE SPACES TO PLAIN-TEXT
           MOVE 1 TO J

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH OF CIPHER-TEXT
               MOVE CIPHER-TEXT(I:1) TO TEMP-CHAR
               
               IF TEMP-CHAR IS ALPHABETIC
                   COMPUTE CHAR-VALUE = FUNCTION ORD(TEMP-CHAR)
                   
                   IF TEMP-CHAR >= "A" AND TEMP-CHAR <= "Z"
                       COMPUTE CHAR-VALUE = CHAR-VALUE - 65
                       COMPUTE CHAR-VALUE = (CHAR-VALUE - SHIFT-VALUE) MOD 26
                       COMPUTE CHAR-VALUE = CHAR-VALUE + 65
                       MOVE FUNCTION CHR(CHAR-VALUE) TO NEW-CHAR
                   ELSE
                       COMPUTE CHAR-VALUE = CHAR-VALUE - 97
                       COMPUTE CHAR-VALUE = (CHAR-VALUE - SHIFT-VALUE) MOD 26
                       COMPUTE CHAR-VALUE = CHAR-VALUE + 97
                       MOVE FUNCTION CHR(CHAR-VALUE) TO NEW-CHAR
                   END-IF
               ELSE
                   MOVE TEMP-CHAR TO NEW-CHAR
               END-IF

               MOVE NEW-CHAR TO PLAIN-TEXT(J:1)
               ADD 1 TO J
           END-PERFORM.
```

## How it works:

1. **Input/Output**: Reads from an input file and writes to an output file
2. **Encryption**: Shifts each letter by a fixed number of positions (default is 3)
3. **Decryption**: Reverses the shift to recover original text
4. **Case Preservation**: Maintains uppercase and lowercase letters
5. **Non-alphabetic characters**: Leaves spaces, punctuation, and numbers unchanged

## Sample Input/Output:

**Input line**: "Hello World!"
**Output line**: "Khoor Zruog!"

## Key COBOL Features Used:

- **FUNCTION ORD()**: Gets ASCII value of character
- **FUNCTION CHR()**: Converts ASCII value back to character
- **MOD function**: Handles wrapping around the alphabet
- **PERFORM VARYING**: Loop control structure
- **File handling**: OPEN, READ, WRITE operations
- **Conditional logic**: IF/ELSE statements for character processing

This implementation demonstrates a complete Caesar cipher with both encryption and decryption capabilities in COBOL.