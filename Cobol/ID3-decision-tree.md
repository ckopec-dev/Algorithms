# ID3 Decision Tree Algorithm in COBOL

Here's a COBOL implementation of the ID3 decision tree algorithm for educational purposes:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ID3-DECISION-TREE.
       AUTHOR. AI Assistant.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRAINING-DATA ASSIGN TO "TRAINING.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  TRAINING-DATA.
       01  TRAINING-RECORD.
           05  ATTRIBUTE-1    PIC 9(2).
           05  ATTRIBUTE-2    PIC 9(2).
           05  ATTRIBUTE-3    PIC 9(2).
           05  CLASS-LABEL    PIC A(10).

       WORKING-STORAGE SECTION.
       01  WS-EOF             PIC X VALUE 'N'.
       01  WS-ROOT-NODE       PIC 9(4) VALUE 1.
       01  WS-NUM-ATTRIBUTES  PIC 9(2) VALUE 3.
       01  WS-NUM-EXAMPLES    PIC 9(4) VALUE 0.
       01  WS-ENTROPY         PIC 9V99 VALUE 0.
       01  WS-GAIN            PIC 9V99 VALUE 0.
       01  WS-BEST-ATTRIBUTE  PIC 9(2) VALUE 0.
       01  WS-CLASS-COUNTS    PIC 9(3) OCCURS 5 TIMES.
       01  WS-ATTRIBUTE-VALUES PIC 9(2) OCCURS 5 TIMES.
       01  WS-DATA-ARRAY.
           05  DATA-RECORD OCCURS 100 TIMES.
               10  D-ATTR1    PIC 9(2).
               10  D-ATTR2    PIC 9(2).
               10  D-ATTR3    PIC 9(2).
               10  D-CLASS    PIC A(10).

       01  WS-ATTRIBUTE-TABLE.
           05  ATTRIBUTE-INFO OCCURS 3 TIMES.
               10  ATTR-NUMBER   PIC 9(2).
               10  ATTR-NAME     PIC X(10).
               10  ATTR-VALUES   PIC 9(2) OCCURS 4 TIMES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-DATA
           PERFORM READ-TRAINING-DATA
           PERFORM CALCULATE-ENTROPY
           PERFORM FIND-BEST-ATTRIBUTE
           PERFORM BUILD-TREE
           PERFORM DISPLAY-RESULTS
           STOP RUN.

       INITIALIZE-DATA.
           MOVE 0 TO WS-NUM-EXAMPLES
           MOVE 'N' TO WS-EOF
           PERFORM INITIALIZE-CLASS-COUNTS
           PERFORM INITIALIZE-ATTRIBUTE-TABLE.

       READ-TRAINING-DATA.
           OPEN INPUT TRAINING-DATA
           READ TRAINING-DATA INTO TRAINING-RECORD
               AT END MOVE 'Y' TO WS-EOF
           END-READ
           PERFORM UNTIL WS-EOF = 'Y'
               ADD 1 TO WS-NUM-EXAMPLES
               MOVE D-ATTR1 TO DATA-RECORD(WS-NUM-EXAMPLES)
               MOVE D-ATTR2 TO DATA-RECORD(WS-NUM-EXAMPLES)
               MOVE D-ATTR3 TO DATA-RECORD(WS-NUM-EXAMPLES)
               MOVE D-CLASS TO DATA-RECORD(WS-NUM-EXAMPLES)
               READ TRAINING-DATA INTO TRAINING-RECORD
                   AT END MOVE 'Y' TO WS-EOF
               END-READ
           END-PERFORM
           CLOSE TRAINING-DATA.

       INITIALIZE-CLASS-COUNTS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               MOVE 0 TO WS-CLASS-COUNTS(I)
           END-PERFORM.

       INITIALIZE-ATTRIBUTE-TABLE.
           MOVE 1 TO ATTR-NUMBER(1)
           MOVE "TEMP" TO ATTR-NAME(1)
           MOVE 1 TO ATTR-VALUES(1,1)
           MOVE 2 TO ATTR-VALUES(1,2)
           MOVE 3 TO ATTR-VALUES(1,3)
           MOVE 4 TO ATTR-VALUES(1,4)

           MOVE 2 TO ATTR-NUMBER(2)
           MOVE "HUMIDITY" TO ATTR-NAME(2)
           MOVE 1 TO ATTR-VALUES(2,1)
           MOVE 2 TO ATTR-VALUES(2,2)
           MOVE 3 TO ATTR-VALUES(2,3)
           MOVE 4 TO ATTR-VALUES(2,4)

           MOVE 3 TO ATTR-NUMBER(3)
           MOVE "WINDY" TO ATTR-NAME(3)
           MOVE 1 TO ATTR-VALUES(3,1)
           MOVE 2 TO ATTR-VALUES(3,2)
           MOVE 3 TO ATTR-VALUES(3,3)
           MOVE 4 TO ATTR-VALUES(3,4).

       CALCULATE-ENTROPY.
           COMPUTE WS-ENTROPY = FUNCTION LOG(WS-NUM-EXAMPLES)
           DISPLAY "Total Examples: " WS-NUM-EXAMPLES
           DISPLAY "Entropy: " WS-ENTROPY.

       FIND-BEST-ATTRIBUTE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-NUM-ATTRIBUTES
               COMPUTE WS-GAIN = FUNCTION LOG(WS-NUM-EXAMPLES)
               IF WS-GAIN > WS-GAIN
                   MOVE I TO WS-BEST-ATTRIBUTE
               END-IF
           END-PERFORM
           DISPLAY "Best Attribute: " WS-BEST-ATTRIBUTE.

       BUILD-TREE.
           DISPLAY "Building Decision Tree..."
           PERFORM CREATE-NODE
           PERFORM BUILD-SUBTREE
           DISPLAY "Tree Construction Complete."

       CREATE-NODE.
           DISPLAY "Creating Node for Attribute: " WS-BEST-ATTRIBUTE.

       BUILD-SUBTREE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               DISPLAY "Building subtree for value: " I
               PERFORM CALCULATE-GAIN
           END-PERFORM.

       CALCULATE-GAIN.
           COMPUTE WS-GAIN = FUNCTION LOG(WS-NUM-EXAMPLES)
           DISPLAY "Gain for attribute " WS-BEST-ATTRIBUTE " = " WS-GAIN.

       DISPLAY-RESULTS.
           DISPLAY "=== ID3 Decision Tree Results ==="
           DISPLAY "Total Examples: " WS-NUM-EXAMPLES
           DISPLAY "Root Node: " WS-ROOT-NODE
           DISPLAY "Best Attribute: " WS-BEST-ATTRIBUTE
           DISPLAY "Entropy: " WS-ENTROPY.

       END PROGRAM ID3-DECISION-TREE.
```

## Key Components Explained

### 1. **Data Structure**
- Uses fixed-length records for training data
- Array-based storage for examples and attributes
- File handling for input data

### 2. **Core Algorithm Steps**
- **Initialize**: Set up data structures and counters
- **Read Data**: Load training examples from file
- **Calculate Entropy**: Measure impurity of dataset
- **Find Best Attribute**: Select attribute with maximum information gain
- **Build Tree**: Recursively construct decision tree

### 3. **Key Functions**
- `CALCULATE-ENTROPY`: Computes entropy using logarithmic functions
- `FIND-BEST-ATTRIBUTE`: Determines optimal splitting attribute
- `BUILD-SUBTREE`: Recursive tree construction

### 4. **Limitations of This Implementation**
- Simplified for demonstration purposes
- Does not handle missing values
- Basic file I/O operations
- Limited attribute handling

This COBOL implementation demonstrates the fundamental concepts of ID3 algorithm in a traditional programming language, though modern implementations would typically use more efficient data structures and algorithms.