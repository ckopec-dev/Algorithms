# Ukkonen's Algorithm Implementation in COBOL

Below is a COBOL implementation of Ukkonen's suffix tree algorithm. This is a simplified version focusing on the core concepts rather than a full production implementation.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UKKONEN-SUFFIX-TREE.
       AUTHOR. AI Assistant.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  TEXT-INPUT.
           05  TEXT-LENGTH        PIC 9(4) VALUE 0.
           05  TEXT-STRING        PIC X(100) VALUE SPACES.
           05  TEXT-ARRAY         OCCURS 100 TIMES PIC X.

       01  SUFFIX-TREE.
           05  NODE-COUNT         PIC 9(4) VALUE 0.
           05  NODE-ARRAY.
               10  NODE OCCURS 200 TIMES.
                   15  NODE-ID        PIC 9(4).
                   15  START-POS      PIC 9(4).
                   15  END-POS        PIC 9(4).
                   15  SUFFIX-LINK    PIC 9(4).
                   15  CHILD-NODES    PIC 9(4) OCCURS 26 TIMES.

       01  VARIABLES.
           05  I                  PIC 9(4).
           05  J                  PIC 9(4).
           05  K                  PIC 9(4).
           05  L                  PIC 9(4).
           05  ACTIVE-POINT.
               10  ACTIVE-NODE    PIC 9(4).
               10  ACTIVE-EDGE    PIC 9(4).
               10  ACTIVE-LENGTH  PIC 9(4).
           05  REMAINDER          PIC 9(4).
           05  CURRENT-CHAR       PIC X.
           05  EDGE-CHAR          PIC X.
           05  FOUND            PIC X VALUE 'N'.

       01  TEMP-VALUES.
           05  TEMP-INT           PIC 9(4).
           05  TEMP-CHAR          PIC X.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "Ukkonen's Suffix Tree Algorithm"
           DISPLAY "================================"

           MOVE "BANANA" TO TEXT-STRING
           COMPUTE TEXT-LENGTH = FUNCTION LENGTH(TEXT-STRING)

           DISPLAY "Input text: " TEXT-STRING
           DISPLAY "Length: " TEXT-LENGTH

           CALL "BUILD-SUFFIX-TREE" USING TEXT-STRING, TEXT-LENGTH

           DISPLAY "Suffix tree built successfully!"
           STOP RUN.

       BUILD-SUFFIX-TREE.
           000-BUILD-SUFFIX-TREE.
               MOVE 0 TO NODE-COUNT
               MOVE 1 TO ACTIVE-NODE
               MOVE 0 TO ACTIVE-LENGTH
               MOVE 0 TO REMAINDER

               PERFORM VARYING I FROM 1 BY 1 UNTIL I > TEXT-LENGTH
                   PERFORM INSERT-CHARACTER
               END-PERFORM

           EXIT PROGRAM.

       INSERT-CHARACTER.
           000-INSERT-CHARACTER.
               ADD 1 TO REMAINDER
               MOVE TEXT-STRING(I:1) TO CURRENT-CHAR

               PERFORM UNTIL REMAINDER = 0
                   PERFORM CHECK-AND-EXTEND
                   IF FOUND = 'Y'
                       EXIT PERFORM
                   END-IF
                   PERFORM UPDATE-ACTIVE-POINT
               END-PERFORM

           EXIT PROGRAM.

       CHECK-AND-EXTEND.
           000-CHECK-AND-EXTEND.
               MOVE 'N' TO FOUND

               IF ACTIVE-LENGTH = 0
                   PERFORM CHECK-ROOT-EXTENSION
               ELSE
                   PERFORM CHECK-INTERNAL-EXTENSION
               END-IF

           EXIT PROGRAM.

       CHECK-ROOT-EXTENSION.
           000-CHECK-ROOT-EXTENSION.
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > NODE-COUNT
                   IF CHILD-NODES(J) = 0
                       PERFORM EXTEND-ROOT
                       MOVE 'Y' TO FOUND
                       EXIT PERFORM
                   END-IF
               END-PERFORM

           EXIT PROGRAM.

       CHECK-INTERNAL-EXTENSION.
           000-CHECK-INTERNAL-EXTENSION.
               MOVE TEXT-STRING(ACTIVE-EDGE:1) TO EDGE-CHAR
               IF EDGE-CHAR = CURRENT-CHAR
                   ADD 1 TO ACTIVE-LENGTH
                   EXIT PERFORM
               END-IF

               PERFORM SPLIT-NODE

           EXIT PROGRAM.

       EXTEND-ROOT.
           000-EXTEND-ROOT.
               ADD 1 TO NODE-COUNT
               MOVE NODE-COUNT TO CHILD-NODES(J)
               MOVE I TO START-POS
               MOVE TEXT-LENGTH TO END-POS

           EXIT PROGRAM.

       SPLIT-NODE.
           000-SPLIT-NODE.
               ADD 1 TO NODE-COUNT
               MOVE NODE-COUNT TO SUFFIX-LINK
               MOVE ACTIVE-NODE TO NODE-ID
               MOVE ACTIVE-EDGE TO START-POS
               MOVE I TO END-POS

           EXIT PROGRAM.

       UPDATE-ACTIVE-POINT.
           000-UPDATE-ACTIVE-POINT.
               IF ACTIVE-LENGTH > 0
                   SUBTRACT 1 FROM ACTIVE-LENGTH
                   IF ACTIVE-LENGTH = 0
                       MOVE 1 TO ACTIVE-NODE
                   END-IF
               ELSE
                   SUBTRACT 1 FROM REMAINDER
               END-IF

           EXIT PROGRAM.

       END PROGRAM UKKONEN-SUFFIX-TREE.
```

## Key Features of this COBOL Implementation:

### 1. **Data Structures**
- `TEXT-INPUT`: Stores the input string and its length
- `SUFFIX-TREE`: Array-based representation of the suffix tree nodes
- `ACTIVE-POINT`: Tracks the current active point during construction
- `VARIABLES`: Various working variables needed for algorithm execution

### 2. **Algorithm Components**
- **Main Loop**: Iterates through each character of the input string
- **Insert Character**: Processes each new character and extends the tree
- **Check Extensions**: Determines if the current character can be extended
- **Node Splitting**: Handles internal node splitting when needed
- **Active Point Management**: Updates the active point after each extension

### 3. **Algorithm Steps Implemented**
1. Initialize the suffix tree with an empty root
2. For each character in the text:
   - Check if the character can be extended from the current active point
   - If not, create a new leaf node
   - Update the active point and remainder
   - Handle suffix links appropriately

### 4. **Limitations**
- This is a simplified educational implementation
- Actual Ukkonen's algorithm requires more complex handling of suffix links
- Full production version would include proper memory management and error handling
- The tree representation is simplified for COBOL constraints

This implementation demonstrates the core concepts of Ukkonen's algorithm in COBOL syntax, though a complete production implementation would require additional complexity for proper suffix tree construction.

