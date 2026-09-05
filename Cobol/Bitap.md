# Bitap Algorithm Implementation in COBOL

The following is an example implementation of the Bitap algorithm (also known as the Shift-Or algorithm) for string pattern matching in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BITAP-ALGORITHM.
       AUTHOR. COBOL IMPLEMENTATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  WS-TEXT-LENGTH        PIC 9(4) VALUE 0.
       01  WS-PATTERN-LENGTH     PIC 9(4) VALUE 0.
       01  WS-TEXT               PIC X(100).
       01  WS-PATTERN            PIC X(50).
       01  WS-ALPHABET-SIZE      PIC 9(4) VALUE 256.
       
       01  WS-BITMAP-TABLE.
           05  WS-BITMAP OCCURS 256 TIMES PIC 9(8).
       
       01  WS-MASK               PIC 9(8) VALUE 0.
       01  WS-STATE              PIC 9(8) VALUE 0.
       01  WS-RESULT             PIC 9(4) VALUE 0.
       01  WS-I                  PIC 9(4) VALUE 0.
       01  WS-J                  PIC 9(4) VALUE 0.
       01  WS-CHAR               PIC X.
       01  WS-FOUND              PIC X VALUE 'N'.
       01  WS-TEXT-POS           PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "BITAP ALGORITHM DEMONSTRATION"
           DISPLAY "============================="
           
           MOVE "ABCDABCABD" TO WS-TEXT
           MOVE "ABC" TO WS-PATTERN
           
           COMPUTE WS-TEXT-LENGTH = FUNCTION LENGTH(WS-TEXT)
           COMPUTE WS-PATTERN-LENGTH = FUNCTION LENGTH(WS-PATTERN)
           
           DISPLAY "Text: " WS-TEXT
           DISPLAY "Pattern: " WS-PATTERN
           DISPLAY "Text Length: " WS-TEXT-LENGTH
           DISPLAY "Pattern Length: " WS-PATTERN-LENGTH
           
           PERFORM INITIALIZE-BITMAP
           PERFORM SEARCH-PATTERN
           
           IF WS-FOUND = 'Y'
               DISPLAY "Pattern found at position: " WS-RESULT
           ELSE
               DISPLAY "Pattern not found"
           END-IF
           
           STOP RUN.

       INITIALIZE-BITMAP.
           *> Initialize bitmap table to 0
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-ALPHABET-SIZE
               MOVE 0 TO WS-BITMAP(WS-I)
           END-PERFORM.
           
           *> Set bits for pattern characters
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-PATTERN-LENGTH
               MOVE WS-PATTERN(WS-I:1) TO WS-CHAR
               COMPUTE WS-J = FUNCTION ORD(WS-CHAR)
               COMPUTE WS-BITMAP(WS-J) = WS-BITMAP(WS-J) OR (1 << (WS-PATTERN-LENGTH - WS-I))
           END-PERFORM.

       SEARCH-PATTERN.
           *> Initialize state to 0
           MOVE 0 TO WS-STATE
           MOVE 'N' TO WS-FOUND
           MOVE 0 TO WS-RESULT
           
           *> Process each character of text
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-TEXT-LENGTH
               MOVE WS-TEXT(WS-I:1) TO WS-CHAR
               COMPUTE WS-J = FUNCTION ORD(WS-CHAR)
               
               *> Update state using bit operations
               COMPUTE WS-STATE = (WS-STATE << 1) OR 1
               COMPUTE WS-STATE = WS-STATE AND WS-BITMAP(WS-J)
               
               *> Check if pattern is found
               IF WS-STATE >= 0 AND WS-STATE < (1 << WS-PATTERN-LENGTH)
                   COMPUTE WS-RESULT = WS-I - WS-PATTERN-LENGTH + 1
                   MOVE 'Y' TO WS-FOUND
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       SEARCH-PATTERN-ALT.
           *> Alternative implementation with more explicit bit checking
           MOVE 0 TO WS-STATE
           MOVE 'N' TO WS-FOUND
           MOVE 0 TO WS-RESULT
           
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-TEXT-LENGTH
               MOVE WS-TEXT(WS-I:1) TO WS-CHAR
               COMPUTE WS-J = FUNCTION ORD(WS-CHAR)
               
               *> Bit manipulation for pattern matching
               COMPUTE WS-STATE = (WS-STATE << 1) OR 1
               COMPUTE WS-STATE = WS-STATE AND WS-BITMAP(WS-J)
               
               *> Check if we have a complete match
               COMPUTE WS-MASK = (1 << WS-PATTERN-LENGTH) - 1
               IF WS-STATE = WS-MASK
                   COMPUTE WS-RESULT = WS-I - WS-PATTERN-LENGTH + 1
                   MOVE 'Y' TO WS-FOUND
                   EXIT PERFORM
               END-IF
           END-PERFORM.
```

## Key Features of this Implementation:

### **Algorithm Components:**
1. **Bitmap Initialization**: Creates a lookup table for pattern characters
2. **State Tracking**: Uses bit operations to track potential matches
3. **Pattern Matching**: Implements the core Bitap algorithm logic

### **COBOL-Specific Features:**
- Uses `FUNCTION ORD` for character-to-ASCII conversion
- Implements bit manipulation using arithmetic operations
- Handles string length calculations with `FUNCTION LENGTH`
- Uses `PERFORM VARYING` loops for iteration
- Implements conditional logic with proper IF/ELSE structures

### **Algorithm Logic:**
1. **Preprocessing**: Build bitmap table of pattern characters
2. **Searching**: Process text character by character using bit operations
3. **Matching**: Check if the final state indicates a complete match

This implementation demonstrates how to apply the Bitap algorithm in COBOL, which is particularly useful for string searching tasks where performance is important due to its linear time complexity.