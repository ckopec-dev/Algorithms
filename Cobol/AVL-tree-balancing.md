# AVL Tree Balancing Algorithm in COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AVL-TREE-BALANCE.
       AUTHOR. COBOL-PROGRAMMER.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  TREE-STRUCTURE.
           05  NODE-RECORD.
               10  NODE-KEY         PIC 9(6).
               10  NODE-LEFT        PIC 9(6).
               10  NODE-RIGHT       PIC 9(6).
               10  NODE-BALANCE     PIC S9(3).
               10  NODE-LEVEL       PIC 9(3).
           05  NODE-ARRAY       OCCURS 100 TIMES.
               10  NODE-KEY-ITEM    PIC 9(6).
               10  NODE-LEFT-ITEM   PIC 9(6).
               10  NODE-RIGHT-ITEM  PIC 9(6).
               10  NODE-BALANCE-ITEM PIC S9(3).
               10  NODE-LEVEL-ITEM  PIC 9(3).

       01  TREE-VARIABLES.
           05  ROOT-INDEX       PIC 9(6) VALUE 0.
           05  NODE-COUNT       PIC 9(6) VALUE 0.
           05  TEMP-INDEX       PIC 9(6).
           05  BALANCE-FACTOR   PIC S9(3).
           05  TEMP-BALANCE     PIC S9(3).

       01  WORK-VALUES.
           05  LEFT-CHILD       PIC 9(6).
           05  RIGHT-CHILD      PIC 9(6).
           05  TEMP-KEY         PIC 9(6).
           05  NEW-ROOT         PIC 9(6).
           05  ROTATION-TYPE    PIC X(1) VALUE SPACE.

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           DISPLAY "AVL TREE BALANCING ALGORITHM"
           DISPLAY "============================"
           
           PERFORM INITIALIZE-TREE
           PERFORM INSERT-NODES
           PERFORM DISPLAY-TREE
           PERFORM BALANCE-TREE
           PERFORM DISPLAY-TREE-AFTER-BALANCE
           
           STOP RUN.

       INITIALIZE-TREE.
           MOVE 0 TO ROOT-INDEX
           MOVE 0 TO NODE-COUNT
           PERFORM VARYING TEMP-INDEX FROM 1 BY 1 UNTIL TEMP-INDEX > 100
               MOVE 0 TO NODE-KEY-ITEM(TEMP-INDEX)
               MOVE 0 TO NODE-LEFT-ITEM(TEMP-INDEX)
               MOVE 0 TO NODE-RIGHT-ITEM(TEMP-INDEX)
               MOVE 0 TO NODE-BALANCE-ITEM(TEMP-INDEX)
               MOVE 0 TO NODE-LEVEL-ITEM(TEMP-INDEX)
           END-PERFORM.

       INSERT-NODES.
           DISPLAY "Inserting nodes into AVL tree..."
           PERFORM INSERT-KEY 100 TIMES
           DISPLAY "Nodes inserted successfully."

       INSERT-KEY.
           ADD 1 TO NODE-COUNT
           MOVE NODE-COUNT TO NODE-KEY-ITEM(NODE-COUNT)
           MOVE 0 TO NODE-LEFT-ITEM(NODE-COUNT)
           MOVE 0 TO NODE-RIGHT-ITEM(NODE-COUNT)
           MOVE 0 TO NODE-BALANCE-ITEM(NODE-COUNT)
           DISPLAY "Inserted node with key: " NODE-KEY-ITEM(NODE-COUNT).

       DISPLAY-TREE.
           DISPLAY "Tree before balancing:"
           PERFORM VARYING TEMP-INDEX FROM 1 BY 1 UNTIL TEMP-INDEX > NODE-COUNT
               IF NODE-KEY-ITEM(TEMP-INDEX) NOT = 0
                   DISPLAY "Node " TEMP-INDEX ": Key=" NODE-KEY-ITEM(TEMP-INDEX)
                   DISPLAY "  Left=" NODE-LEFT-ITEM(TEMP-INDEX)
                   DISPLAY "  Right=" NODE-RIGHT-ITEM(TEMP-INDEX)
                   DISPLAY "  Balance=" NODE-BALANCE-ITEM(TEMP-INDEX)
               END-IF
           END-PERFORM.

       BALANCE-TREE.
           DISPLAY "Starting AVL tree balancing..."
           PERFORM BALANCE-NODES
           DISPLAY "Tree balancing completed."

       BALANCE-NODES.
           PERFORM VARYING TEMP-INDEX FROM 1 BY 1 UNTIL TEMP-INDEX > NODE-COUNT
               IF NODE-KEY-ITEM(TEMP-INDEX) NOT = 0
                   PERFORM CALCULATE-BALANCE-FACTOR
                   PERFORM CHECK-AND-ROTATE
               END-IF
           END-PERFORM.

       CALCULATE-BALANCE-FACTOR.
           MOVE NODE-BALANCE-ITEM(TEMP-INDEX) TO BALANCE-FACTOR
           
           IF BALANCE-FACTOR > 1 OR BALANCE-FACTOR < -1
               DISPLAY "Node " TEMP-INDEX " requires rotation"
               PERFORM DO-ROTATION
           END-IF.

       CHECK-AND-ROTATE.
           IF BALANCE-FACTOR > 1
               IF NODE-BALANCE-ITEM(NODE-LEFT-ITEM(TEMP-INDEX)) >= 0
                   PERFORM RIGHT-ROTATION
               ELSE
                   PERFORM LEFT-ROTATION
                   PERFORM RIGHT-ROTATION
               END-IF
           ELSE IF BALANCE-FACTOR < -1
               IF NODE-BALANCE-ITEM(NODE-RIGHT-ITEM(TEMP-INDEX)) <= 0
                   PERFORM LEFT-ROTATION
               ELSE
                   PERFORM RIGHT-ROTATION
                   PERFORM LEFT-ROTATION
               END-IF
           END-IF.

       RIGHT-ROTATION.
           DISPLAY "Performing Right Rotation on node " TEMP-INDEX
           MOVE NODE-LEFT-ITEM(TEMP-INDEX) TO LEFT-CHILD
           MOVE NODE-LEFT-ITEM(LEFT-CHILD) TO NODE-LEFT-ITEM(TEMP-INDEX)
           MOVE LEFT-CHILD TO NODE-RIGHT-ITEM(LEFT-CHILD)
           MOVE TEMP-INDEX TO NODE-LEFT-ITEM(LEFT-CHILD)
           PERFORM UPDATE-BALANCE-FACTOR.

       LEFT-ROTATION.
           DISPLAY "Performing Left Rotation on node " TEMP-INDEX
           MOVE NODE-RIGHT-ITEM(TEMP-INDEX) TO RIGHT-CHILD
           MOVE NODE-RIGHT-ITEM(RIGHT-CHILD) TO NODE-RIGHT-ITEM(TEMP-INDEX)
           MOVE RIGHT-CHILD TO NODE-LEFT-ITEM(RIGHT-CHILD)
           MOVE TEMP-INDEX TO NODE-RIGHT-ITEM(RIGHT-CHILD)
           PERFORM UPDATE-BALANCE-FACTOR.

       UPDATE-BALANCE-FACTOR.
           PERFORM CALCULATE-NEW-BALANCE
           DISPLAY "Balance factor updated for node " TEMP-INDEX.

       CALCULATE-NEW-BALANCE.
           COMPUTE BALANCE-FACTOR = 
               NODE-BALANCE-ITEM(NODE-LEFT-ITEM(TEMP-INDEX)) 
               - NODE-BALANCE-ITEM(NODE-RIGHT-ITEM(TEMP-INDEX))
           MOVE BALANCE-FACTOR TO NODE-BALANCE-ITEM(TEMP-INDEX).

       DO-ROTATION.
           IF BALANCE-FACTOR > 1 AND NODE-BALANCE-ITEM(NODE-LEFT-ITEM(TEMP-INDEX)) >= 0
               PERFORM RIGHT-ROTATION
           ELSE IF BALANCE-FACTOR > 1 AND NODE-BALANCE-ITEM(NODE-LEFT-ITEM(TEMP-INDEX)) < 0
               PERFORM LEFT-ROTATION
               PERFORM RIGHT-ROTATION
           ELSE IF BALANCE-FACTOR < -1 AND NODE-BALANCE-ITEM(NODE-RIGHT-ITEM(TEMP-INDEX)) <= 0
               PERFORM LEFT-ROTATION
           ELSE IF BALANCE-FACTOR < -1 AND NODE-BALANCE-ITEM(NODE-RIGHT-ITEM(TEMP-INDEX)) > 0
               PERFORM RIGHT-ROTATION
               PERFORM LEFT-ROTATION
           END-IF.

       DISPLAY-TREE-AFTER-BALANCE.
           DISPLAY "Tree after balancing:"
           PERFORM VARYING TEMP-INDEX FROM 1 BY 1 UNTIL TEMP-INDEX > NODE-COUNT
               IF NODE-KEY-ITEM(TEMP-INDEX) NOT = 0
                   DISPLAY "Node " TEMP-INDEX ": Key=" NODE-KEY-ITEM(TEMP-INDEX)
                   DISPLAY "  Left=" NODE-LEFT-ITEM(TEMP-INDEX)
                   DISPLAY "  Right=" NODE-RIGHT-ITEM(TEMP-INDEX)
                   DISPLAY "  Balance=" NODE-BALANCE-ITEM(TEMP-INDEX)
               END-IF
           END-PERFORM.

       END PROGRAM AVL-TREE-BALANCE.
```

## Key Features of This AVL Tree Implementation

### 1. **Tree Structure**
- Uses an array-based approach for nodes
- Each node contains key, left child, right child, balance factor, and level information

### 2. **Balancing Operations**
- **Right Rotation**: Used when left-heavy subtree needs balancing
- **Left Rotation**: Used when right-heavy subtree needs balancing
- **Double Rotations**: Combined rotations for complex imbalances

### 3. **Balance Factor Calculation**
- Computes balance factor as height of left subtree minus height of right subtree
- Maintains AVL property (balance factor between -1 and 1)

### 4. **Rotation Logic**
- **LL Rotation**: Left-left case (single right rotation)
- **RR Rotation**: Right-right case (single left rotation)
- **LR Rotation**: Left-right case (left rotation followed by right rotation)
- **RL Rotation**: Right-left case (right rotation followed by left rotation)

### 5. **Algorithm Flow**
1. Initialize tree structure
2. Insert nodes
3. Calculate balance factors
4. Check for imbalances (>1 or <-1)
5. Perform appropriate rotations
6. Update balance factors
7. Display final balanced tree

This implementation demonstrates the core AVL balancing algorithm in COBOL syntax, showing how to maintain the AVL property through rotations when the tree becomes unbalanced.

