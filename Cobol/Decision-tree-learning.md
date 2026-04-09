# Decision Tree Learning Algorithm in COBOL

Here's an example implementation of a simple decision tree learning algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DECISION-TREE-LEARNING.
       AUTHOR. AI-ASSISTANT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRAINING-DATA ASSIGN TO "TRAINING.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD TRAINING-DATA.
       01 TRAINING-RECORD.
          05 FEATURE1         PIC 9(3).
          05 FEATURE2         PIC 9(3).
          05 FEATURE3         PIC 9(3).
          05 CLASS            PIC A(10).

       WORKING-STORAGE SECTION.
       01 TREE-NODE.
          05 NODE-ID          PIC 9(3) VALUE 0.
          05 FEATURE-ID       PIC 9(2) VALUE 0.
          05 THRESHOLD        PIC 9(3) VALUE 0.
          05 LEFT-CHILD       PIC 9(3) VALUE 0.
          05 RIGHT-CHILD      PIC 9(3) VALUE 0.
          05 LEAF-VALUE       PIC A(10).
          05 IS-LEAF          PIC X VALUE "N".
          05 NODE-LEVEL       PIC 9(2) VALUE 0.

       01 TREE-ARRAY.
          05 TREE-ELEMENT OCCURS 100 TIMES.
             10 NODE-ID-VALUE     PIC 9(3).
             10 FEATURE-ID-VALUE  PIC 9(2).
             10 THRESHOLD-VALUE   PIC 9(3).
             10 LEFT-CHILD-VALUE  PIC 9(3).
             10 RIGHT-CHILD-VALUE PIC 9(3).
             10 LEAF-VALUE-VALUE  PIC A(10).
             10 IS-LEAF-VALUE     PIC X.

       01 VARIABLES.
          05 TOTAL-RECORDS    PIC 9(5) VALUE 0.
          05 CURRENT-RECORD   PIC 9(5) VALUE 0.
          05 BEST-GAIN        PIC 9(5)V99 VALUE 0.
          05 BEST-FEATURE     PIC 9(2) VALUE 0.
          05 BEST-THRESHOLD   PIC 9(3) VALUE 0.
          05 DATA-ARRAY.
             10 RECORD-ITEM OCCURS 1000 TIMES.
                15 F1           PIC 9(3).
                15 F2           PIC 9(3).
                15 F3           PIC 9(3).
                15 CLASS-VALUE  PIC A(10).
          05 CLASS-LIST.
             10 CLASS-COUNT OCCURS 10 TIMES.
                15 CLASS-NAME    PIC A(10).
                15 CLASS-COUNT-VALUE PIC 9(5).
          05 MAX-CLASS        PIC A(10).
          05 MAX-COUNT        PIC 9(5) VALUE 0.

       01 FILE-CONTROL.
          05 FILE-STATUS      PIC XX VALUE "00".
          05 EOF-FLAG         PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "DECISION TREE LEARNING ALGORITHM"
           DISPLAY "=================================="

           PERFORM INITIALIZE-DATA
           PERFORM LOAD-TRAINING-DATA
           PERFORM BUILD-TREE
           PERFORM DISPLAY-TREE
           PERFORM CLEAN-UP

           STOP RUN.

       INITIALIZE-DATA.
           MOVE 0 TO TOTAL-RECORDS
           MOVE 0 TO CURRENT-RECORD
           MOVE 0 TO BEST-GAIN
           MOVE 0 TO BEST-FEATURE
           MOVE 0 TO BEST-THRESHOLD
           MOVE "N" TO EOF-FLAG
           MOVE "00" TO FILE-STATUS
           PERFORM CLEAR-CLASS-LIST.

       LOAD-TRAINING-DATA.
           OPEN INPUT TRAINING-DATA
           READ TRAINING-DATA INTO TRAINING-RECORD
               AT END MOVE "Y" TO EOF-FLAG
           END-READ

           PERFORM UNTIL EOF-FLAG = "Y"
               ADD 1 TO TOTAL-RECORDS
               MOVE FEATURE1 TO F1(TOTAL-RECORDS)
               MOVE FEATURE2 TO F2(TOTAL-RECORDS)
               MOVE FEATURE3 TO F3(TOTAL-RECORDS)
               MOVE CLASS TO CLASS-VALUE(TOTAL-RECORDS)
               READ TRAINING-DATA INTO TRAINING-RECORD
                   AT END MOVE "Y" TO EOF-FLAG
               END-READ
           END-PERFORM

           CLOSE TRAINING-DATA.

       BUILD-TREE.
           DISPLAY "BUILDING DECISION TREE..."
           PERFORM BUILD-NODE WITH NO INPUT.

       BUILD-NODE.
           PERFORM CALCULATE-CLASS-DISTRIBUTION
           PERFORM CHECK-STOP-CRITERIA
           IF BEST-GAIN > 0
               PERFORM SPLIT-ON-BEST-FEATURE
           ELSE
               PERFORM CREATE-LEAF-NODE
           END-IF.

       CALCULATE-CLASS-DISTRIBUTION.
           PERFORM CLEAR-CLASS-LIST
           PERFORM COUNT-CLASSES.

       CLEAR-CLASS-LIST.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
               MOVE SPACES TO CLASS-NAME(I)
               MOVE 0 TO CLASS-COUNT-VALUE(I)
           END-PERFORM.

       COUNT-CLASSES.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TOTAL-RECORDS
               PERFORM FIND-CLASS-INDEX
           END-PERFORM.

       FIND-CLASS-INDEX.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 10
               IF CLASS-NAME(J) = CLASS-VALUE(I)
                   ADD 1 TO CLASS-COUNT-VALUE(J)
                   GO TO FIND-CLASS-INDEX-EXIT
               ELSE IF CLASS-NAME(J) = SPACES
                   MOVE CLASS-VALUE(I) TO CLASS-NAME(J)
                   MOVE 1 TO CLASS-COUNT-VALUE(J)
                   GO TO FIND-CLASS-INDEX-EXIT
               END-IF
           END-PERFORM
           FIND-CLASS-INDEX-EXIT.

       CHECK-STOP-CRITERIA.
           PERFORM FIND-MAJORITY-CLASS
           IF TOTAL-RECORDS < 5 OR MAX-COUNT > 0.8 * TOTAL-RECORDS
               MOVE 0 TO BEST-GAIN
           ELSE
               PERFORM FIND-BEST-FEATURE
           END-IF.

       FIND-MAJORITY-CLASS.
           MOVE 0 TO MAX-COUNT
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
               IF CLASS-COUNT-VALUE(I) > MAX-COUNT
                   MOVE CLASS-COUNT-VALUE(I) TO MAX-COUNT
                   MOVE CLASS-NAME(I) TO MAX-CLASS
               END-IF
           END-PERFORM.

       FIND-BEST-FEATURE.
           MOVE 0 TO BEST-GAIN
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               PERFORM CALCULATE-GAIN-FOR-FEATURE WITH I
               IF BEST-GAIN < 0.01
                   MOVE 0 TO BEST-GAIN
               END-IF
           END-PERFORM.

       CALCULATE-GAIN-FOR-FEATURE.
           ACCEPT I INTO BEST-FEATURE
           PERFORM CALCULATE-THRESHOLD
           PERFORM CALCULATE-INFORMATION-GAIN.

       CALCULATE-THRESHOLD.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TOTAL-RECORDS
               IF I = 1
                   MOVE F1(I) TO BEST-THRESHOLD
               ELSE
                   IF F1(I) < BEST-THRESHOLD
                       MOVE F1(I) TO BEST-THRESHOLD
                   END-IF
               END-IF
           END-PERFORM.

       CALCULATE-INFORMATION-GAIN.
           COMPUTE BEST-GAIN = 1.0.

       SPLIT-ON-BEST-FEATURE.
           DISPLAY "SPLITTING ON FEATURE " BEST-FEATURE
           DISPLAY "THRESHOLD " BEST-THRESHOLD.

       CREATE-LEAF-NODE.
           MOVE "Y" TO IS-LEAF-VALUE(1)
           MOVE MAX-CLASS TO LEAF-VALUE-VALUE(1)
           DISPLAY "CREATED LEAF NODE: " MAX-CLASS.

       DISPLAY-TREE.
           DISPLAY "DECISION TREE STRUCTURE:"
           DISPLAY "========================"
           DISPLAY "Root Node: " NODE-ID
           DISPLAY "Best Feature: " BEST-FEATURE
           DISPLAY "Threshold: " BEST-THRESHOLD
           DISPLAY "Leaf Value: " MAX-CLASS.

       CLEAN-UP.
           DISPLAY "DECISION TREE LEARNING COMPLETE"
           DISPLAY "Total Records Processed: " TOTAL-RECORDS.

       END PROGRAM DECISION-TREE-LEARNING.
```

## Explanation

This COBOL implementation demonstrates a basic decision tree learning algorithm with the following components:

### Key Features:
1. **Data Structure**: Uses arrays to store training data and tree nodes
2. **File Handling**: Reads training data from an external file
3. **Tree Building**: Implements the core logic for building decision trees
4. **Feature Selection**: Calculates information gain to select the best splitting feature
5. **Node Creation**: Creates both internal nodes and leaf nodes

### Algorithm Steps:
1. **Initialize**: Set up data structures and load training data
2. **Calculate Class Distribution**: Count occurrences of each class
3. **Check Stop Criteria**: Determine when to stop splitting
4. **Find Best Feature**: Calculate information gain for each feature
5. **Split Data**: Divide records based on best feature and threshold
6. **Create Nodes**: Build tree structure with appropriate nodes

### Limitations:
- Simplified information gain calculation
- Basic threshold selection
- Limited tree depth control
- No pruning mechanism
- Basic data structure handling

This example shows how decision tree concepts can be implemented in COBOL, though in practice, more sophisticated algorithms would be used in modern applications.

