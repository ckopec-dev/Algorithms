# Push-Relabel Max-Flow Algorithm in COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PUSHRELABEL-MAXFLOW.
       AUTHOR. COBOL IMPLEMENTATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "GRAPH.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
          05 SOURCE-NODE    PIC 9(3).
          05 DEST-NODE      PIC 9(3).
          05 CAPACITY       PIC 9(5).

       WORKING-STORAGE SECTION.
       01 GRAPH-STRUCTURE.
          05 NODES          PIC 9(3) VALUE 0.
          05 EDGES          PIC 9(4) VALUE 0.
          05 MAX-NODES      PIC 9(3) VALUE 100.
          05 MAX-EDGES      PIC 9(4) VALUE 1000.

       01 FLOW-MATRIX.
          05 FLOW-TABLE     OCCURS 100 TIMES INDEXED BY I-J.
             10 FLOW-VALUE  PIC 9(6) VALUE 0.

       01 RESIDUAL-MATRIX.
          05 RESIDUAL-TABLE OCCURS 100 TIMES INDEXED BY I-K.
             10 RESIDUAL-VALUE PIC 9(6) VALUE 0.

       01 HEIGHT-MATRIX.
          05 HEIGHT-TABLE   OCCURS 100 TIMES INDEXED BY I-L.
             10 HEIGHT-VALUE   PIC 9(3) VALUE 0.

       01 EXCESS-MATRIX.
          05 EXCESS-TABLE   OCCURS 100 TIMES INDEXED BY I-M.
             10 EXCESS-VALUE   PIC 9(6) VALUE 0.

       01 NODE-QUEUE.
          05 QUEUE-TABLE    OCCURS 100 TIMES INDEXED BY I-N.
             10 QUEUE-ITEM     PIC 9(3) VALUE 0.
          05 QUEUE-FRONT    PIC 9(3) VALUE 0.
          05 QUEUE-BACK     PIC 9(3) VALUE 0.
          05 QUEUE-SIZE     PIC 9(3) VALUE 0.

       01 TEMPORARY-VARIABLES.
          05 CURRENT-NODE   PIC 9(3) VALUE 0.
          05 NEIGHBOR       PIC 9(3) VALUE 0.
          05 PUSH-AMOUNT    PIC 9(6) VALUE 0.
          05 MAX-FLOW       PIC 9(9) VALUE 0.
          05 ITERATION      PIC 9(3) VALUE 0.
          05 FOUND-EXCESS   PIC X VALUE 'N'.
          05 QUEUE-EMPTY    PIC X VALUE 'Y'.

       01 FILE-STATUS.
          05 FILE-STATUS-CODE PIC XX VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "PUSH RELABEL MAX FLOW ALGORITHM"
           DISPLAY "=================================="

           PERFORM INITIALIZE-ALGORITHM
           PERFORM READ-GRAPH-DATA
           PERFORM INITIALIZATION-STEP
           PERFORM PUSH-RELABEL-LOOP
           PERFORM DISPLAY-RESULTS

           STOP RUN.

       INITIALIZE-ALGORITHM.
           MOVE 0 TO NODES, EDGES
           MOVE 0 TO MAX-FLOW
           MOVE 0 TO QUEUE-FRONT, QUEUE-BACK, QUEUE-SIZE
           MOVE 'N' TO FOUND-EXCESS, QUEUE-EMPTY
           MOVE 0 TO ITERATION

       READ-GRAPH-DATA.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE AT END GO TO READ-END
           PERFORM PROCESS-EDGE
           GO TO READ-GRAPH-DATA
           READ-END.
           CLOSE INPUT-FILE.

       PROCESS-EDGE.
           ADD 1 TO EDGES
           IF SOURCE-NODE > NODES
              MOVE SOURCE-NODE TO NODES
           END-IF
           IF DEST-NODE > NODES
              MOVE DEST-NODE TO NODES
           END-IF
           PERFORM STORE-EDGE

       STORE-EDGE.
           COMPUTE I-J = (SOURCE-NODE * 100) + DEST-NODE
           MOVE CAPACITY TO FLOW-TABLE(I-J)
           MOVE CAPACITY TO RESIDUAL-TABLE(I-J)
           COMPUTE I-K = (DEST-NODE * 100) + SOURCE-NODE
           MOVE 0 TO FLOW-TABLE(I-K)
           MOVE 0 TO RESIDUAL-TABLE(I-K)

       INITIALIZATION-STEP.
           MOVE 0 TO HEIGHT-TABLE(1)
           MOVE NODES TO HEIGHT-TABLE(NODES)
           PERFORM INITIALIZE-EXCESS

       INITIALIZE-EXCESS.
           MOVE 0 TO EXCESS-TABLE(1)
           MOVE 0 TO EXCESS-TABLE(NODES)
           MOVE 0 TO EXCESS-TABLE(2)
           MOVE 0 TO EXCESS-TABLE(3)
           MOVE 0 TO EXCESS-TABLE(4)
           MOVE 0 TO EXCESS-TABLE(5)

       PUSH-RELABEL-LOOP.
           PERFORM UNTIL QUEUE-EMPTY = 'Y'
               PERFORM GET-NEXT-NODE
               IF CURRENT-NODE > 0
                   PERFORM PUSH-FLOW
                   PERFORM RELABEL-NODE
               ELSE
                   MOVE 'Y' TO QUEUE-EMPTY
               END-IF
           END-PERFORM

       GET-NEXT-NODE.
           IF QUEUE-SIZE > 0
               MOVE QUEUE-ITEM(QUEUE-FRONT) TO CURRENT-NODE
               COMPUTE QUEUE-FRONT = QUEUE-FRONT + 1
               IF QUEUE-FRONT > MAX-NODES
                   MOVE 1 TO QUEUE-FRONT
               END-IF
               COMPUTE QUEUE-SIZE = QUEUE-SIZE - 1
               MOVE 'N' TO QUEUE-EMPTY
           ELSE
               MOVE 0 TO CURRENT-NODE
               MOVE 'Y' TO QUEUE-EMPTY
           END-IF

       PUSH-FLOW.
           IF EXCESS-VALUE(CURRENT-NODE) > 0
               PERFORM UNTIL EXCESS-VALUE(CURRENT-NODE) = 0
                   PERFORM FIND-NEIGHBOR
                   IF NEIGHBOR > 0
                       PERFORM CALCULATE-PUSH-AMOUNT
                       IF PUSH-AMOUNT > 0
                           PERFORM EXECUTE-PUSH
                       END-IF
                   ELSE
                       GO TO PUSH-END
                   END-IF
               END-PERFORM
           END-IF
           PUSH-END.

       FIND-NEIGHBOR.
           MOVE 0 TO NEIGHBOR
           PERFORM VARYING I = 1 BY 1 UNTIL I > NODES OR NEIGHBOR > 0
               COMPUTE I-K = (CURRENT-NODE * 100) + I
               IF RESIDUAL-TABLE(I-K) > 0
                  AND HEIGHT-TABLE(CURRENT-NODE) > HEIGHT-TABLE(I)
                  MOVE I TO NEIGHBOR
               END-IF
           END-PERFORM

       CALCULATE-PUSH-AMOUNT.
           COMPUTE PUSH-AMOUNT = MIN(EXCESS-VALUE(CURRENT-NODE),
                                    RESIDUAL-TABLE(I-K))

       EXECUTE-PUSH.
           COMPUTE FLOW-VALUE(I-J) = FLOW-VALUE(I-J) + PUSH-AMOUNT
           COMPUTE RESIDUAL-VALUE(I-J) = RESIDUAL-VALUE(I-J) - PUSH-AMOUNT
           COMPUTE RESIDUAL-VALUE(J-I) = RESIDUAL-VALUE(J-I) + PUSH-AMOUNT
           COMPUTE EXCESS-VALUE(CURRENT-NODE) = EXCESS-VALUE(CURRENT-NODE) - PUSH-AMOUNT
           COMPUTE EXCESS-VALUE(NEIGHBOR) = EXCESS-VALUE(NEIGHBOR) + PUSH-AMOUNT
           IF EXCESS-VALUE(NEIGHBOR) > 0
               AND NEIGHBOR NOT = 1
               AND NEIGHBOR NOT = NODES
               PERFORM ADD-TO-QUEUE(NEIGHBOR)
           END-IF

       ADD-TO-QUEUE.
           COMPUTE QUEUE-BACK = QUEUE-BACK + 1
           IF QUEUE-BACK > MAX-NODES
               MOVE 1 TO QUEUE-BACK
           END-IF
           MOVE CURRENT-NODE TO QUEUE-ITEM(QUEUE-BACK)
           COMPUTE QUEUE-SIZE = QUEUE-SIZE + 1

       RELABEL-NODE.
           IF EXCESS-VALUE(CURRENT-NODE) > 0
               PERFORM RELABEL-LOOP
           END-IF

       RELABEL-LOOP.
           COMPUTE HEIGHT-TABLE(CURRENT-NODE) = HEIGHT-TABLE(CURRENT-NODE) + 1
           PERFORM UNTIL HEIGHT-TABLE(CURRENT-NODE) >= NODES
               COMPUTE I-K = (CURRENT-NODE * 100) + I
               IF RESIDUAL-TABLE(I-K) > 0
                   AND HEIGHT-TABLE(I) > HEIGHT-TABLE(CURRENT-NODE)
                       COMPUTE HEIGHT-TABLE(CURRENT-NODE) = HEIGHT-TABLE(I) + 1
                       GO TO RELABEL-END
                   END-IF
               END-PERFORM
           RELABEL-END.

       DISPLAY-RESULTS.
           DISPLAY "MAXIMUM FLOW: " MAX-FLOW
           DISPLAY "ALGORITHM COMPLETED SUCCESSFULLY"
           DISPLAY "=================================="

       END PROGRAM PUSHRELABEL-MAXFLOW.
```

## Example Input File (GRAPH.DAT)
```
1 2 10
1 3 10
2 3 2
2 4 4
3 4 6
3 5 10
4 5 10
```

## Algorithm Explanation

This COBOL implementation of the Push-Relabel max-flow algorithm includes:

1. **Data Structures**: 
   - Flow matrix for tracking flow values
   - Residual matrix for remaining capacities
   - Height matrix for node heights
   - Excess matrix for excess flow at each node
   - Queue for active nodes

2. **Key Operations**:
   - **Initialization**: Sets up the graph and initial heights
   - **Push**: Transfers flow from high to low nodes when possible
   - **Relabel**: Increases node height when no more pushes are possible
   - **Main Loop**: Continues until no more active nodes exist

3. **Time Complexity**: O(V²E) where V is vertices and E is edges

The algorithm efficiently computes the maximum flow from source to sink in a flow network using the push-relabel technique.

