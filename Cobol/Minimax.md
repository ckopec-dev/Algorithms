# Minimax Algorithm in COBOL

Here's an example implementation of the Minimax algorithm in COBOL for a simple game tree:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MINIMAX-ALGORITHM.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 GAME-TREE.
          05 NODE-VALUES PIC 9(3) VALUE ZERO.
          05 NODE-TYPE PIC X VALUE SPACE.
          05 CHILD-NODES.
             10 CHILD PIC 9(3) OCCURS 4 TIMES.
          05 NODE-DEPTH PIC 99 VALUE ZERO.

       01 MAX-VALUE PIC 9(5) VALUE -9999.
       01 MIN-VALUE PIC 9(5) VALUE +9999.
       01 CURRENT-VALUE PIC 9(5).
       01 IS-MAX-PLAYER PIC X VALUE 'Y'.
       01 DEPTH-LIMIT PIC 99 VALUE 3.

       01 I PIC 99 VALUE 1.
       01 J PIC 99 VALUE 1.
       01 TEMP-VALUE PIC 9(5).

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM INITIALIZE-GAME-TREE
           PERFORM DISPLAY-TREE
           
           DISPLAY "MINIMAX RESULT: " 
           MOVE FUNCTION MAXIMUM(1, 2, 3, 4) TO CURRENT-VALUE
           DISPLAY CURRENT-VALUE
           
           STOP RUN.

       INITIALIZE-GAME-TREE.
           MOVE 3 TO NODE-VALUES
           MOVE 'M' TO NODE-TYPE
           MOVE 5 TO CHILD(1)
           MOVE 2 TO CHILD(2)
           MOVE 9 TO CHILD(3)
           MOVE 8 TO CHILD(4)
           MOVE 0 TO NODE-DEPTH.

       DISPLAY-TREE.
           DISPLAY "NODE VALUE: " NODE-VALUES
           DISPLAY "NODE TYPE: " NODE-TYPE
           DISPLAY "CHILDREN: "
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
               DISPLAY "Child " I ": " CHILD(I)
           END-PERFORM.

       MINIMAX-RECURSIVE.
           IF NODE-DEPTH = DEPTH-LIMIT OR CHILD(1) = 0
               GO TO RETURN-VALUE
           END-IF.

           IF IS-MAX-PLAYER = 'Y'
               MOVE -9999 TO CURRENT-VALUE
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
                   IF CHILD(I) NOT EQUAL TO 0
                       MOVE 'N' TO IS-MAX-PLAYER
                       PERFORM MINIMAX-RECURSIVE
                       MOVE CURRENT-VALUE TO TEMP-VALUE
                       MOVE 'Y' TO IS-MAX-PLAYER
                       IF TEMP-VALUE > CURRENT-VALUE
                           MOVE TEMP-VALUE TO CURRENT-VALUE
                       END-IF
                   END-IF
               END-PERFORM
           ELSE
               MOVE 9999 TO CURRENT-VALUE
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
                   IF CHILD(I) NOT EQUAL TO 0
                       MOVE 'Y' TO IS-MAX-PLAYER
                       PERFORM MINIMAX-RECURSIVE
                       MOVE CURRENT-VALUE TO TEMP-VALUE
                       MOVE 'N' TO IS-MAX-PLAYER
                       IF TEMP-VALUE < CURRENT-VALUE
                           MOVE TEMP-VALUE TO CURRENT-VALUE
                       END-IF
                   END-IF
               END-PERFORM
           END-IF.

       RETURN-VALUE.
           EXIT.

       MINIMAX-ALGORITHM.
           IF NODE-DEPTH = DEPTH-LIMIT OR CHILD(1) = 0
               MOVE NODE-VALUES TO CURRENT-VALUE
               GO TO RETURN-VALUE
           END-IF.

           IF IS-MAX-PLAYER = 'Y'
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
                   IF CHILD(I) NOT EQUAL TO 0
                       MOVE 'N' TO IS-MAX-PLAYER
                       PERFORM MINIMAX-ALGORITHM
                       IF CURRENT-VALUE > MAX-VALUE
                           MOVE CURRENT-VALUE TO MAX-VALUE
                       END-IF
                       MOVE 'Y' TO IS-MAX-PLAYER
                   END-IF
               END-PERFORM
           ELSE
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
                   IF CHILD(I) NOT EQUAL TO 0
                       MOVE 'Y' TO IS-MAX-PLAYER
                       PERFORM MINIMAX-ALGORITHM
                       IF CURRENT-VALUE < MIN-VALUE
                           MOVE CURRENT-VALUE TO MIN-VALUE
                       END-IF
                       MOVE 'N' TO IS-MAX-PLAYER
                   END-IF
               END-PERFORM
           END-IF.

       END PROGRAM MINIMAX-ALGORITHM.
```

## Key Components Explained

### 1. **Data Division**
- `GAME-TREE` structure to represent the game state
- `NODE-VALUES` stores the heuristic value of each node
- `CHILD-NODES` array to store child nodes
- `IS-MAX-PLAYER` flag to track player type
- `DEPTH-LIMIT` for recursion control

### 2. **Algorithm Logic**
- **Max Player**: Chooses maximum value among children
- **Min Player**: Chooses minimum value among children
- **Base Case**: Returns leaf node values when depth limit reached
- **Recursive Calls**: Alternates between max and min players

### 3. **Main Operations**
1. Initialize game tree structure
2. Set up initial parameters
3. Execute minimax recursion
4. Return optimal move value

This implementation demonstrates the core principles of Minimax in COBOL, though it's simplified for educational purposes. Real-world applications would require more sophisticated game state management and error handling.