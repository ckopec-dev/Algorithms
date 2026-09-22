# Q-Learning Algorithm in COBOL

Below is an example implementation of a Q-learning algorithm in COBOL. This example demonstrates a simple reinforcement learning scenario where an agent learns to navigate a grid world.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. QLEARNING-ALGORITHM.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-GAME-BOARD.
          05  WS-BOARD PIC 9(1) OCCURS 4 TIMES INDEXED BY IX.
       
       01  WS-Q-TABLE.
          05  WS-Q-VALUES PIC 9(3)V99 OCCURS 16 TIMES INDEXED BY IJ.
       
       01  WS-AGENT-POSITION PIC 9(1).
       01  WS-ACTION PIC 9(1).
       01  WS-REWARD PIC 9(2).
       01  WS-LEARNING-RATE PIC 9V9 VALUE 0.8.
       01  WS-DISCOUNT-GAMMA PIC 9V9 VALUE 0.9.
       01  WS-EXPLORATION-RATE PIC 9V9 VALUE 0.2.
       01  WS-EPOCHS PIC 9(4) VALUE 1000.
       01  WS-EPISODE-COUNT PIC 9(4).
       01  WS-STEP-COUNT PIC 9(3).
       01  WS-RANDOM-NUMBER PIC 9V9.

       01  WS-TERMINATED PIC X VALUE 'N'.
           88  TERMINATED VALUE 'Y'.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM INITIALIZE-VALUES
           PERFORM TRAIN-AGENT
           PERFORM DISPLAY-RESULTS
           STOP RUN.

       INITIALIZE-VALUES.
           MOVE 0 TO WS-AGENT-POSITION
           MOVE 0 TO WS-EPISODE-COUNT
           
           *> Initialize Q-table with zeros
           PERFORM VARYING IJ FROM 1 BY 1 UNTIL IJ > 16
               MOVE 0 TO WS-Q-VALUES(IJ)
           END-PERFORM

           *> Initialize board (0=empty, 1=goal, 2=obstacle)
           MOVE 0 TO WS-BOARD(1)   *> Start state
           MOVE 0 TO WS-BOARD(2)   *> Empty
           MOVE 1 TO WS-BOARD(3)   *> Goal
           MOVE 2 TO WS-BOARD(4)   *> Obstacle

           DISPLAY "Q-Learning Initialized"
           .

       TRAIN-AGENT.
           PERFORM VARYING WS-EPISODE-COUNT FROM 1 BY 1 
               UNTIL WS-EPISODE-COUNT > WS-EPOCHS
               PERFORM RESET-AGENT
               PERFORM RUN-EPISODE
           END-PERFORM
           .

       RESET-AGENT.
           MOVE 1 TO WS-AGENT-POSITION
           MOVE 'N' TO WS-TERMINATED
           MOVE 0 TO WS-STEP-COUNT
           .

       RUN-EPISODE.
           PERFORM UNTIL TERMINATED
               PERFORM SELECT-ACTION
               PERFORM TAKE-ACTION
               PERFORM UPDATE-Q-TABLE
               PERFORM CHECK-TERMINATION
           END-PERFORM
           .

       SELECT-ACTION.
           *> Epsilon-greedy action selection
           COMPUTE WS-RANDOM-NUMBER = FUNCTION RANDOM * 100
           
           IF WS-RANDOM-NUMBER < (WS-EXPLORATION-RATE * 100)
               *> Explore - random action
               COMPUTE WS-ACTION = FUNCTION RANDOM * 4 + 1
           ELSE
               *> Exploit - best known action
               PERFORM GET-BEST-ACTION
           END-IF
           .

       GET-BEST-ACTION.
           *> Find action with highest Q-value for current state
           MOVE 1 TO WS-ACTION
           COMPUTE IJ = (WS-AGENT-POSITION - 1) * 4 + 1
           MOVE WS-Q-VALUES(IJ) TO WS-REWARD
           
           PERFORM VARYING IJ FROM 2 BY 1 UNTIL IJ > 4
               COMPUTE IJ = (WS-AGENT-POSITION - 1) * 4 + IJ
               IF WS-Q-VALUES(IJ) > WS-REWARD
                   MOVE IJ TO WS-ACTION
               END-IF
           END-PERFORM
           .

       TAKE-ACTION.
           *> Move agent based on selected action
           COMPUTE WS-AGENT-POSITION = WS-AGENT-POSITION + WS-ACTION
           IF WS-AGENT-POSITION > 4 OR WS-AGENT-POSITION < 1
               MOVE WS-AGENT-POSITION TO WS-REWARD
               COMPUTE WS-AGENT-POSITION = WS-REWARD
           END-IF
           .

       UPDATE-Q-TABLE.
           *> Q-learning update rule:
           *> Q(s,a) = Q(s,a) + α[r + γ * max(Q(s',a')) - Q(s,a)]
           
           COMPUTE IJ = (WS-AGENT-POSITION - 1) * 4 + WS-ACTION
           COMPUTE WS-REWARD = 0
           
           *> Calculate reward based on current position
           IF WS-BOARD(WS-AGENT-POSITION) = 1
               MOVE 10 TO WS-REWARD
           ELSE IF WS-BOARD(WS-AGENT-POSITION) = 2
               MOVE -10 TO WS-REWARD
           END-IF
           
           *> Find max Q-value for next state
           COMPUTE IJ = (WS-AGENT-POSITION - 1) * 4 + 1
           MOVE WS-Q-VALUES(IJ) TO WS-REWARD
           
           PERFORM VARYING IJ FROM 2 BY 1 UNTIL IJ > 4
               COMPUTE IJ = (WS-AGENT-POSITION - 1) * 4 + IJ
               IF WS-Q-VALUES(IJ) > WS-REWARD
                   MOVE WS-Q-VALUES(IJ) TO WS-REWARD
               END-IF
           END-PERFORM
           
           *> Update Q-value
           COMPUTE IJ = (WS-AGENT-POSITION - 1) * 4 + WS-ACTION
           COMPUTE WS-Q-VALUES(IJ) =
               WS-Q-VALUES(IJ) + 
               WS-LEARNING-RATE * 
               (WS-REWARD + 
                WS-DISCOUNT-GAMMA * WS-REWARD - 
                WS-Q-VALUES(IJ))
           .

       CHECK-TERMINATION.
           ADD 1 TO WS-STEP-COUNT
           IF WS-AGENT-POSITION = 3 OR WS-STEP-COUNT > 100
               SET TERMINATED TO TRUE
           END-IF
           .

       DISPLAY-RESULTS.
           DISPLAY "Training completed"
           DISPLAY "Final Q-table:"
           
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 4
               COMPUTE IJ = (IX - 1) * 4 + 1
               DISPLAY "State ", IX, ": ", 
                       WS-Q-VALUES(IJ), " ", 
                       WS-Q-VALUES(IJ+1), " ", 
                       WS-Q-VALUES(IJ+2), " ", 
                       WS-Q-VALUES(IJ+3)
           END-PERFORM
           .
```

## Key Components of the COBOL Q-Learning Implementation

### 1. **Data Structures**
- `WS-GAME-BOARD`: Represents the environment states (0=empty, 1=goal, 2=obstacle)
- `WS-Q-TABLE`: 4x4 Q-table storing state-action values
- Various working storage variables for learning parameters

### 2. **Core Algorithm Steps**
- **Initialization**: Set up board and Q-table
- **Episode Loop**: Run multiple episodes of agent interaction
- **Action Selection**: Epsilon-greedy strategy (exploration vs exploitation)
- **State Transition**: Agent moves based on selected action
- **Q-Value Update**: Apply Bellman equation update rule

### 3. **Q-Learning Formula Implementation**
```
Q(s,a) = Q(s,a) + α[r + γ * max(Q(s',a')) - Q(s,a)]
```

This COBOL implementation demonstrates how reinforcement learning concepts can be applied using traditional programming constructs, though it's worth noting that modern implementations would typically use more efficient languages like Python or R for such algorithms.