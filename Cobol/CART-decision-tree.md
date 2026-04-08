Below is a **simplified** implementation of the CART (Classification and Regression Trees) decision‑tree algorithm written in **COBOL**.  
It is intended as a teaching example – real‑world data handling, error checking, and performance optimisations would be much more elaborate.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CART-DecisionTree.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       * ------------------------------------------------------------
       *  Parameters
       * ------------------------------------------------------------
       01  MAX-ATTRIBUTES   PIC 9(2) VALUE 5.      *> number of input attributes
       01  MAX-RECORDS      PIC 9(4) VALUE 1000.   *> maximum training rows
       01  MIN-SAMPLES-LEAF PIC 9(4) VALUE 10.     *> stop splitting when node size < this
       01  MAX-DEPTH        PIC 9(2) VALUE 5.      *> maximum tree depth

       * ------------------------------------------------------------
       *  Training data (example: Iris‑like dataset)
       * ------------------------------------------------------------
       01  TRAIN-DATA.
           05  RECORD OCCURS MAX-RECORDS TIMES.
               10  ATTRIBUTE PIC S9(4)V99 OCCURS MAX-ATTRIBUTES TIMES.
               10  CLASS-LABEL PIC X(10).               *> e.g. "SETOSA", "VERSIC", "VIRGIN"

       * ------------------------------------------------------------
       *  Working structures
       * ------------------------------------------------------------
       01  NODE.
           05  NODE-ID            PIC 9(4) COMP.
           05  IS-LEAF            PIC X   VALUE 'N'.
           05  PREDICTED-CLASS    PIC X(10).
           05  SPLIT-ATTRIBUTE    PIC 9(2) COMP.
           05  SPLIT-THRESHOLD    PIC S9(4)V99.
           05  LEFT-CHILD         PIC 9(4) COMP.
           05  RIGHT-CHILD        PIC 9(4) COMP.
           05  SAMPLE-COUNT       PIC 9(5) COMP.
           05  CLASS-COUNTS.
               10  COUNT-OCCURS OCCURS 3 TIMES PIC 9(4) COMP. *> assumes 3 classes

       01  NODE-POOL.
           05  NODE-ENTRY OCCURS 1000 TIMES.
               10  NODE-REC  LIKE NODE.

       * ------------------------------------------------------------
       *  Procedure Division
       * ------------------------------------------------------------
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           MOVE ZERO TO NODE-POOL-LENGTH.
           PERFORM BUILD-TREE
               VARYING NODE-ID FROM 1 BY 1
               UNTIL NODE-ID > MAX-NODES
           END-PERFORM.
           DISPLAY "Tree built. Root node ID = " NODE-POOL(1).NODE-ID.
           STOP RUN.

       * ------------------------------------------------------------
       *  Build tree recursively (depth‑first)
       * ------------------------------------------------------------
       BUILD-TREE.
           IF NODE-ID > MAX-NODES
               MOVE 'Y' TO NODE-POOL(NODE-ID).IS-LEAF
               MOVE ZERO TO NODE-POOL(NODE-ID).PREDICTED-CLASS
               GO TO EXIT-BUILD
           END-IF

           MOVE NODE-ID TO NODE-POOL(NODE-ID).NODE-ID
           MOVE ZERO TO NODE-POOL(NODE-ID).SAMPLE-COUNT
           MOVE ZEROS TO NODE-POOL(NODE-ID).CLASS-COUNTS

           * --- Count samples and class frequencies for this node ---
           PERFORM VARYING REC-INDEX FROM 1 BY 1
                   UNTIL REC-INDEX > MAX-RECORDS
               IF SAMPLE-BELONGS-TO-NODE(REC-INDEX, NODE-ID)
                   ADD 1 TO NODE-POOL(NODE-ID).SAMPLE-COUNT
                   MOVE CLASS-LABEL(REC-INDEX) TO TEMP-CLASS
                   PERFORM INCREMENT-CLASS-COUNT
               END-IF
           END-PERFORM

           * --- Stopping criteria ---
           IF NODE-POOL(NODE-ID).SAMPLE-COUNT < MIN-SAMPLES-LEAF
               OR NODE-DEPTH(NODE-ID) >= MAX-DEPTH
               MOVE 'Y' TO NODE-POOL(NODE-ID).IS-LEAF
               MOVE MAJORITY-CLASS(NODE-ID) TO NODE-POOL(NODE-ID).PREDICTED-CLASS
               GO TO EXIT-BUILD
           END-IF

           * --- Find best split (impurity reduction = Gini) ---
           PERFORM FIND-BEST-SPLIT
               GIVING BEST-ATTR BEST-THRESH BEST-GAIN
           END-PERFORM

           IF BEST-GAIN = ZERO
               MOVE 'Y' TO NODE-POOL(NODE-ID).IS-LEAF
               MOVE MAJORITY-CLASS(NODE-ID) TO NODE-POOL(NODE-ID).PREDICTED-CLASS
               GO TO EXIT-BUILD
           END-IF

           MOVE BEST-ATTR TO NODE-POOL(NODE-ID).SPLIT-ATTRIBUTE
           MOVE BEST-THRESH TO NODE-POOL(NODE-ID).SPLIT-THRESHOLD
           MOVE 'N' TO NODE-POOL(NODE-ID).IS-LEAF

           * --- Create child nodes ---
           ADD 1 TO LEFT-CHILD-ID   GIVING LEFT-CHILD-ID
           ADD 1 TO RIGHT-CHILD-ID  GIVING RIGHT-CHILD-ID
           MOVE LEFT-CHILD-ID  TO NODE-POOL(NODE-ID).LEFT-CHILD
           MOVE RIGHT-CHILD-ID TO NODE-POOL(NODE-ID).RIGHT-CHILD

           * --- Recurse on left and right subsets ---
           PERFORM BUILD-TREE
               WITH NODE-ID = LEFT-CHILD-ID
               WITH DEPTH   = NODE-DEPTH(NODE-ID) + 1
           END-PERFORM
           PERFORM BUILD-TREE
               WITH NODE-ID = RIGHT-CHILD-ID
               WITH DEPTH   = NODE-DEPTH(NODE-ID) + 1
           END-PERFORM

       EXIT-BUILD.
           EXIT.

       * ------------------------------------------------------------
       *  Helper: check if a record belongs to the current node
       * ------------------------------------------------------------
       SAMPLE-BELONGS-TO-NODE.
           * Simple implementation: all records are considered for the root;
           * for sub‑nodes we would filter based on parent split.
           MOVE 'Y' TO RESULT.
           EXIT FUNCTION RESULT.

       * ------------------------------------------------------------
       *  Helper: increment class count for a record
       * ------------------------------------------------------------
       INCREMENT-CLASS-COUNT.
           EVALUATE TEMP-CLASS
               WHEN "SETOSA"   ADD 1 TO NODE-POOL(NODE-ID).CLASS-COUNts(1)
               WHEN "VERSIC"   ADD 1 TO NODE-POOL(NODE-ID).CLASS-COUNts(2)
               WHEN "VIRGIN"   ADD 1 TO NODE-POOL(NODE-ID).CLASS-COUNts(3)
               WHEN OTHER      CONTINUE
           END-EVALUATE.
           EXIT.

       * ------------------------------------------------------------
       *  Helper: compute majority class for a node
       * ------------------------------------------------------------
       MAJORITY-CLASS.
           MOVE ZERO TO MAX-COUNT
           MOVE SPACES TO MAJ-CLASS
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               IF NODE-POOL(NODE-ID).CLASS-COUNts(I) > MAX-COUNT
                   MOVE NODE-POOL(NODE-ID).CLASS-COUNts(I) TO MAX-COUNT
                   MOVE CLASS-NAME(I) TO MAJ-CLASS
               END-IF
           END-PERFORM
           EXIT FUNCTION MAJ-CLASS.

       * ------------------------------------------------------------
       *  Helper: find best split using Gini impurity
       * ------------------------------------------------------------
       FIND-BEST-SPLIT.
           MOVE ZERO TO BEST-GAIN
           MOVE ZERO TO BEST-ATTR
           MOVE ZERO TO BEST-THRESH
           PERFORM VARYING ATTR-INDEX FROM 1 BY 1
                   UNTIL ATTR-INDEX > MAX-ATTRIBUTES
               * ---- generate candidate thresholds (midpoints) ----
               PERFORM VARYING THRESH-INDEX FROM 1 BY 1
                       UNTIL THRESH-INDEX > NUM-THRESHOLDS(ATTR-INDEX)
                   MOVE THRESH-VAL(ATTR-INDEX, THRESH-INDEX) TO CURR-THRESH
                   COMPUTE GAIN = COMPUTE-GINI-GAIN(NODE-ID, ATTR-INDEX, CURR-THRESH)
                   IF GAIN > BEST-GAIN
                       MOVE GAIN   TO BEST-GAIN
                       MOVE ATTR-INDEX TO BEST-ATTR
                       MOVE CURR-THRESH TO BEST-THRESH
                   END-IF
               END-PERFORM
           END-PERFORM
           EXIT FUNCTION BEST-ATTR BEST-THRESH BEST-GAIN.

       * ------------------------------------------------------------
       *  Helper: compute Gini gain for a split
       * ------------------------------------------------------------
       COMPUTE-GINI-GAIN.
           * (Implementation omitted for brevity – standard Gini formula)
           EXIT FUNCTION GAIN.

       * ------------------------------------------------------------
       *  End of program
       * ------------------------------------------------------------
       END PROGRAM CART-DecisionTree.
```

### How the example works
1. **Data** – A static table `TRAIN-DATA` holds the training records (attributes + class label).  
2. **Node structure** – Each node stores split information, class counts, and pointers to children.  
3. **Recursive building** – `BUILD-TREE` stops when a node is too small, reaches max depth, or no impurity reduction is possible.  
4. **Split selection** – `FIND-BEST-SPLIT` iterates over attributes and candidate thresholds, computing the Gini impurity gain; the best split is stored.  
5. **Prediction** – (not shown) would traverse the tree using the split attribute/threshold until a leaf is reached, then return the leaf’s majority class.

> **Note:** This COBOL snippet is deliberately compact and omits many production‑concerns (dynamic memory, I/O, robust error handling, efficient sorting for thresholds, etc.). It serves as a conceptual illustration of how the CART algorithm could be expressed in COBOL.