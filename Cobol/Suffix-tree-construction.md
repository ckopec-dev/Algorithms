# Suffix Tree Construction in COBOL

Below is an example implementation of suffix tree construction algorithm in COBOL. This implementation demonstrates the basic structure and logic needed for building a suffix tree.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUFFIX-TREE-CONSTRUCTION.
       AUTHOR. COBOL-SUFFIX-TREE-EXAMPLE.

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
       01 TEXT-LENGTH PIC 9(5) VALUE 0.
       01 TEXT-ARRAY.
           05 TEXT-CHAR(1000) PIC X.
       01 SUFFIX-ARRAY.
           05 SUFFIX-RECORD.
               10 SUFFIX-START PIC 9(5).
               10 SUFFIX-LENGTH PIC 9(5).
       01 NODE-STRUCTURE.
           05 NODE-ID PIC 9(5).
           05 NODE-START PIC 9(5).
           05 NODE-END PIC 9(5).
           05 NODE-CHILDREN OCCURS 26 TIMES.
               10 CHILD-NODE PIC 9(5).
       01 TREE-ROOT PIC 9(5) VALUE 1.
       01 CURRENT-NODE PIC 9(5) VALUE 1.
       01 NODE-COUNT PIC 9(5) VALUE 1.
       01 I PIC 9(5) VALUE 0.
       01 J PIC 9(5) VALUE 0.
       01 K PIC 9(5) VALUE 0.
       01 TEMP-CHAR PIC X.
       01 TEMP-LENGTH PIC 9(5) VALUE 0.
       01 EOF-FLAG PIC X VALUE 'N'.
       01 MAX-LENGTH PIC 9(5) VALUE 1000.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "SUFFIX TREE CONSTRUCTION ALGORITHM"
           DISPLAY "====================================="

           PERFORM READ-INPUT-DATA
           PERFORM BUILD-SUFFIX-TREE
           PERFORM DISPLAY-TREE-RESULTS

           STOP RUN.

       READ-INPUT-DATA.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE
               AT END MOVE 'Y' TO EOF-FLAG
           END-READ

           IF EOF-FLAG = 'N'
               PERFORM PROCESS-INPUT-RECORD
           END-IF

           CLOSE INPUT-FILE.

       PROCESS-INPUT-RECORD.
           MOVE INPUT-RECORD TO TEXT-ARRAY
           COMPUTE TEXT-LENGTH = FUNCTION LENGTH(INPUT-RECORD)
           DISPLAY "Input text length: " TEXT-LENGTH
           DISPLAY "Input text: " INPUT-RECORD.

       BUILD-SUFFIX-TREE.
           DISPLAY "Building suffix tree..."
           MOVE 1 TO CURRENT-NODE
           MOVE 1 TO NODE-COUNT

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TEXT-LENGTH
               PERFORM INSERT-SUFFIX I
           END-PERFORM

           DISPLAY "Suffix tree construction completed."
           DISPLAY "Total nodes created: " NODE-COUNT.

       INSERT-SUFFIX.
           01 SUFFIX-START-ARG PIC 9(5) VALUE 0.
           01 SUFFIX-END-ARG PIC 9(5) VALUE 0.
           01 NODE-TO-TRAVEL PIC 9(5) VALUE 0.
           01 CHAR-COMPARE PIC X.
           01 CURRENT-POS PIC 9(5) VALUE 0.

           MOVE 1 TO NODE-TO-TRAVEL
           MOVE 1 TO CURRENT-POS
           MOVE I TO SUFFIX-START-ARG
           COMPUTE SUFFIX-END-ARG = TEXT-LENGTH

           PERFORM TRAVERSE-NODES
           PERFORM INSERT-NEW-EDGE.

       TRAVERSE-NODES.
           01 NODE-CHAR PIC X.
           01 EDGE-LENGTH PIC 9(5) VALUE 0.
           01 MATCH-LENGTH PIC 9(5) VALUE 0.

           PERFORM UNTIL NODE-TO-TRAVEL = 0 OR CURRENT-POS > SUFFIX-END-ARG
               IF NODE-TO-TRAVEL = 0
                   EXIT PARAGRAPH
               END-IF

               COMPUTE EDGE-LENGTH = NODE-END(NODE-TO-TRAVEL) - NODE-START(NODE-TO-TRAVEL) + 1
               COMPUTE MATCH-LENGTH = 0

               PERFORM VARYING K FROM NODE-START(NODE-TO-TRAVEL) BY 1 UNTIL K > NODE-END(NODE-TO-TRAVEL)
                   IF K > SUFFIX-START-ARG + CURRENT-POS - 1
                       EXIT PERFORM
                   END-IF

                   IF TEXT-CHAR(K) = TEXT-CHAR(SUFFIX-START-ARG + CURRENT-POS - 1)
                       ADD 1 TO MATCH-LENGTH
                   ELSE
                       EXIT PERFORM
                   END-IF
               END-PERFORM

               IF MATCH-LENGTH = EDGE-LENGTH
                   COMPUTE CURRENT-POS = CURRENT-POS + EDGE-LENGTH
                   MOVE CHILD-NODE(NODE-TO-TRAVEL) TO NODE-TO-TRAVEL
               ELSE
                   PERFORM SPLIT-NODE
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       SPLIT-NODE.
           01 NEW-NODE PIC 9(5) VALUE 0.
           01 SPLIT-POINT PIC 9(5) VALUE 0.

           ADD 1 TO NODE-COUNT
           MOVE NODE-COUNT TO NEW-NODE
           COMPUTE SPLIT-POINT = NODE-START(NODE-TO-TRAVEL) + MATCH-LENGTH - 1

           MOVE NODE-START(NODE-TO-TRAVEL) TO NODE-START(NEW-NODE)
           MOVE SPLIT-POINT TO NODE-END(NEW-NODE)
           MOVE CHILD-NODE(NODE-TO-TRAVEL) TO CHILD-NODE(NEW-NODE)

           MOVE NEW-NODE TO CHILD-NODE(NODE-TO-TRAVEL)
           MOVE NODE-START(NODE-TO-TRAVEL) TO NODE-START(NODE-TO-TRAVEL)
           COMPUTE NODE-END(NODE-TO-TRAVEL) = SPLIT-POINT
           MOVE NEW-NODE TO CHILD-NODE(NODE-TO-TRAVEL).

       INSERT-NEW-EDGE.
           01 NEW-NODE PIC 9(5) VALUE 0.

           ADD 1 TO NODE-COUNT
           MOVE NODE-COUNT TO NEW-NODE

           MOVE I TO NODE-START(NEW-NODE)
           MOVE TEXT-LENGTH TO NODE-END(NEW-NODE)
           MOVE 0 TO CHILD-NODE(NEW-NODE)

           MOVE NEW-NODE TO CHILD-NODE(CURRENT-NODE).

       DISPLAY-TREE-RESULTS.
           DISPLAY "Suffix Tree Results:"
           DISPLAY "====================="
           DISPLAY "Root node: " TREE-ROOT
           DISPLAY "Total nodes: " NODE-COUNT
           DISPLAY "Text length: " TEXT-LENGTH
           DISPLAY "Construction completed successfully."

       END PROGRAM SUFFIX-TREE-CONSTRUCTION.
```

## Key Features of This Implementation

### 1. **Data Structures**
- `TEXT-ARRAY`: Stores the input text
- `SUFFIX-ARRAY`: Holds suffix information
- `NODE-STRUCTURE`: Represents tree nodes with children
- `CHILD-NODE`: Array to store child node references

### 2. **Core Algorithms**
- **Suffix Tree Construction**: Builds tree by inserting suffixes one by one
- **Node Traversal**: Navigates through existing tree structure
- **Node Splitting**: Handles cases where new suffix doesn't match existing paths
- **Edge Insertion**: Creates new edges for unmatched suffix portions

### 3. **Main Procedures**
- `READ-INPUT-DATA`: Reads input text from file
- `BUILD-SUFFIX-TREE`: Main construction algorithm
- `INSERT-SUFFIX`: Inserts individual suffixes into tree
- `TRAVERSE-NODES`: Searches existing tree paths
- `SPLIT-NODE`: Splits nodes when needed
- `DISPLAY-TREE-RESULTS`: Shows final results

### 4. **Algorithm Steps**
1. Read input text
2. Initialize tree with root node
3. For each suffix position:
   - Traverse existing tree
   - Split nodes if necessary
   - Insert new edges
4. Display results

This implementation demonstrates the fundamental concepts of suffix tree construction in COBOL, though it's a simplified version that would need additional error handling and optimization for production use.

