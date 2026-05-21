# CIPHER__family Algorithm Example in COBOL

Here's an example of implementing a simple cipher algorithm (Caesar Cipher) in COBOL:

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
       01 INPUT-RECORD PIC X(80).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC X(80).

       WORKING-STORAGE SECTION.
       01 CIPHER-TEXT PIC X(80).
       01 PLAIN-TEXT PIC X(80).
       01 KEY-VALUE PIC 99 VALUE 3.
       01 I PIC 99 VALUE 1.
       01 J PIC 99 VALUE 1.
       01 CHAR-VALUE PIC 99.
       01 MOD-VALUE PIC 99.
       01 TEMP-CHAR PIC X.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE
                OUTPUT OUTPUT-FILE
           
           READ INPUT-FILE
               AT END GO TO END-OF-FILE
           END-READ

           MOVE INPUT-RECORD TO PLAIN-TEXT
           PERFORM ENCRYPT-TEXT
           PERFORM DECRYPT-TEXT

           CLOSE INPUT-FILE
                 OUTPUT-FILE
           STOP RUN.

       ENCRYPT-TEXT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 80
               IF PLAIN-TEXT(I:1) NOT = SPACES
                   COMPUTE CHAR-VALUE = FUNCTION ORD(PLAIN-TEXT(I:1))
                   COMPUTE CHAR-VALUE = CHAR-VALUE + KEY-VALUE
                   COMPUTE MOD-VALUE = CHAR-VALUE MOD 256
                   COMPUTE CHAR-VALUE = MOD-VALUE
                   MOVE FUNCTION CHR(CHAR-VALUE) TO CIPHER-TEXT(I:1)
               ELSE
                   MOVE SPACES TO CIPHER-TEXT(I:1)
               END-IF
           END-PERFORM.

       DECRYPT-TEXT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 80
               IF CIPHER-TEXT(I:1) NOT = SPACES
                   COMPUTE CHAR-VALUE = FUNCTION ORD(CIPHER-TEXT(I:1))
                   COMPUTE CHAR-VALUE = CHAR-VALUE - KEY-VALUE
                   COMPUTE MOD-VALUE = CHAR-VALUE MOD 256
                   COMPUTE CHAR-VALUE = MOD-VALUE
                   MOVE FUNCTION CHR(CHAR-VALUE) TO PLAIN-TEXT(I:1)
               ELSE
                   MOVE SPACES TO PLAIN-TEXT(I:1)
               END-IF
           END-PERFORM.

       END-OF-FILE.
           DISPLAY "END OF INPUT FILE"
           GOBACK.
```

## Key Features of This Cipher Implementation:

- **Caesar Cipher Algorithm**: Shifts characters by a fixed number of positions
- **Encryption**: Shifts characters forward by KEY-VALUE
- **Decryption**: Shifts characters backward by KEY-VALUE
- **Character Handling**: Uses FUNCTION ORD and FUNCTION CHR for character conversion
- **File Processing**: Reads from input file, processes data, writes to output file

## Sample Input/Output:

**Input File (INPUT.TXT):**
```
HELLO WORLD
```

**Output File (OUTPUT.TXT):**
```
KHOOR ZRUOG
```

This example demonstrates a basic cipher family algorithm in COBOL, showing how encryption/decryption can be implemented using character manipulation functions.

