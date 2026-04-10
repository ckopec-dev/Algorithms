# LZW Algorithm Implementation in COBOL

Here's a complete COBOL implementation of the LZW compression algorithm:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LZW-COMPRESSION.
       AUTHOR. LZW ALGORITHM IMPLEMENTATION.

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
       01 INPUT-RECORD PIC X(100).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC X(100).

       WORKING-STORAGE SECTION.
       01 DICTIONARY.
          05 DICTIONARY-ENTRY OCCURS 4096 TIMES.
             10 DICTIONARY-STRING PIC X(255).
             10 DICTIONARY-CODE PIC 9(4).
             10 DICTIONARY-LENGTH PIC 9(3).
          05 DICTIONARY-COUNT PIC 9(4) VALUE 256.
          05 DICTIONARY-EMPTY PIC X VALUE "N".

       01 INPUT-CHARACTER PIC X.
       01 CURRENT-STRING PIC X(255).
       01 CURRENT-LENGTH PIC 9(3) VALUE 0.
       01 CODE-VALUE PIC 9(4) VALUE 0.
       01 LAST-CODE PIC 9(4) VALUE 0.
       01 NEW-CODE PIC 9(4) VALUE 0.
       01 FOUND-FLAG PIC X VALUE "N".
       01 INDEX PIC 9(4) VALUE 0.
       01 TEMP-STRING PIC X(255).
       01 TEMP-LENGTH PIC 9(3) VALUE 0.
       01 EOF-FLAG PIC X VALUE "N".
       01 COMPRESSION-MODE PIC X VALUE "C".
       01 OUTPUT-BUFFER PIC X(100).
       01 BUFFER-LENGTH PIC 9(3) VALUE 0.
       01 OUTPUT-CHARACTER PIC X.

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM INITIALIZE-DICTIONARY
           PERFORM COMPRESS-INPUT
           GOBACK.

       INITIALIZE-DICTIONARY.
           MOVE 256 TO DICTIONARY-COUNT
           PERFORM VARYING INDEX FROM 0 BY 1
               UNTIL INDEX > 255
               MOVE INDEX TO DICTIONARY-CODE(INDEX + 1)
               MOVE INDEX TO DICTIONARY-LENGTH(INDEX + 1)
               MOVE INDEX TO CODE-VALUE
               STRING CODE-VALUE DELIMITED BY SIZE
                   INTO DICTIONARY-STRING(INDEX + 1)
           END-PERFORM.

       COMPRESS-INPUT.
           PERFORM READ-INPUT-CHARACTERS
           PERFORM PROCESS-CHARACTERS
           PERFORM WRITE-OUTPUT.

       READ-INPUT-CHARACTERS.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE AT END MOVE "Y" TO EOF-FLAG
           END-READ.

       PROCESS-CHARACTERS.
           IF EOF-FLAG = "N"
               MOVE INPUT-RECORD(1:1) TO INPUT-CHARACTER
               MOVE INPUT-CHARACTER TO CURRENT-STRING(1:1)
               MOVE 1 TO CURRENT-LENGTH
               MOVE 0 TO LAST-CODE
               PERFORM COMPRESS-LOOP
           END-IF.

       COMPRESS-LOOP.
           IF EOF-FLAG = "Y"
               GO TO END-COMPRESSION
           END-IF.

           PERFORM FIND-STRING-CODE
           IF FOUND-FLAG = "Y"
               MOVE CODE-VALUE TO LAST-CODE
               PERFORM READ-NEXT-CHARACTER
               PERFORM ADD-CHARACTER-TO-STRING
               GO TO COMPRESS-LOOP
           ELSE
               PERFORM ADD-NEW-ENTRY
               PERFORM WRITE-CODE
               MOVE 0 TO LAST-CODE
               PERFORM READ-NEXT-CHARACTER
               IF EOF-FLAG = "N"
                   MOVE INPUT-CHARACTER TO CURRENT-STRING(1:1)
                   MOVE 1 TO CURRENT-LENGTH
               END-IF
               GO TO COMPRESS-LOOP
           END-IF.

       FIND-STRING-CODE.
           MOVE "N" TO FOUND-FLAG
           PERFORM VARYING INDEX FROM 1 BY 1
               UNTIL INDEX > DICTIONARY-COUNT OR FOUND-FLAG = "Y"
               IF CURRENT-LENGTH = DICTIONARY-LENGTH(INDEX)
                   IF CURRENT-STRING = DICTIONARY-STRING(INDEX)
                       MOVE DICTIONARY-CODE(INDEX) TO CODE-VALUE
                       MOVE "Y" TO FOUND-FLAG
                   END-IF
               END-IF
           END-PERFORM.

       READ-NEXT-CHARACTER.
           READ INPUT-FILE AT END MOVE "Y" TO EOF-FLAG
           END-READ
           IF EOF-FLAG = "N"
               MOVE INPUT-RECORD(1:1) TO INPUT-CHARACTER
           END-IF.

       ADD-CHARACTER-TO-STRING.
           IF CURRENT-LENGTH < 255
               ADD 1 TO CURRENT-LENGTH
               MOVE INPUT-CHARACTER TO CURRENT-STRING(CURRENT-LENGTH:1)
           END-IF.

       ADD-NEW-ENTRY.
           IF DICTIONARY-COUNT < 4096
               ADD 1 TO DICTIONARY-COUNT
               MOVE CURRENT-STRING TO DICTIONARY-STRING(DICTIONARY-COUNT)
               MOVE CURRENT-LENGTH TO DICTIONARY-LENGTH(DICTIONARY-COUNT)
               MOVE DICTIONARY-COUNT TO DICTIONARY-CODE(DICTIONARY-COUNT)
           END-IF.

       WRITE-CODE.
           IF LAST-CODE > 0
               STRING "CODE:" LAST-CODE DELIMITED BY SIZE
                   INTO OUTPUT-BUFFER
               STRING OUTPUT-BUFFER " " DELIMITED BY SIZE
                   INTO OUTPUT-BUFFER
           END-IF.

       END-COMPRESSION.
           CLOSE INPUT-FILE OUTPUT-FILE.

       WRITE-OUTPUT.
           OPEN OUTPUT OUTPUT-FILE
           WRITE OUTPUT-RECORD FROM OUTPUT-BUFFER
           CLOSE OUTPUT-FILE.

       END PROGRAM LZW-COMPRESSION.
```

## How the LZW Algorithm Works

This COBOL implementation demonstrates the core principles of LZW compression:

### Key Components:

1. **Dictionary Management**: 
   - Maintains a dictionary of strings and their corresponding codes
   - Starts with 256 entries (ASCII characters 0-255)
   - Dynamically adds new entries during compression

2. **Compression Process**:
   - Reads input character by character
   - Builds strings incrementally
   - Searches dictionary for matching strings
   - Outputs codes for matched strings
   - Adds new string-code pairs to dictionary

3. **Dictionary Structure**:
   - Each entry stores: string, code value, and string length
   - Maximum dictionary size: 4096 entries
   - Code values range from 0-4095

### Example Usage:

For input string "ABABABAB":
1. Initialize dictionary with ASCII values (A=0, B=1)
2. Process "A" → output code 0
3. Process "AB" → output code 2 (new entry)
4. Process "ABA" → output code 3 (new entry)
5. Continue building dictionary and outputting codes

### Key Features:

- **Variable-length dictionary**: Grows dynamically during compression
- **Efficient searching**: Linear search through dictionary entries
- **Memory management**: Tracks dictionary size limits
- **File I/O**: Handles input/output files for processing

This implementation provides a foundation for understanding how LZW compression works, though in practice, more optimized versions would be used for production systems.

