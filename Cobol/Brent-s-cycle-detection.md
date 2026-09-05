# Brent's Cycle Detection Algorithm in COBOL

Here's an implementation of Brent's cycle detection algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BRENT-CYCLE-DETECTION.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CURRENT-VALUE        PIC 9(8) VALUE 0.
       01  TORTOISE             PIC 9(8) VALUE 0.
       01  HARE                 PIC 9(8) VALUE 0.
       01  POWER                PIC 9(8) VALUE 1.
       01  LAMBDA               PIC 9(8) VALUE 0.
       01  CYCLE-START          PIC 9(8) VALUE 0.
       01  FOUND-CYCLE          PIC 9 VALUE 0.
       01  ITERATION            PIC 9(8) VALUE 0.
       01  MAX-ITERATIONS       PIC 9(8) VALUE 1000.

       01  SEQUENCE-TABLE.
           05  SEQ-ELEMENT        PIC 9(8) OCCURS 100 TIMES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Brent's Cycle Detection Algorithm"
           DISPLAY "=================================="

           * Initialize sequence table (example: 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
           MOVE 2 TO SEQ-ELEMENT(1)
           MOVE 3 TO SEQ-ELEMENT(2)
           MOVE 4 TO SEQ-ELEMENT(3)
           MOVE 5 TO SEQ-ELEMENT(4)
           MOVE 6 TO SEQ-ELEMENT(5)
           MOVE 7 TO SEQ-ELEMENT(6)
           MOVE 8 TO SEQ-ELEMENT(7)
           MOVE 9 TO SEQ-ELEMENT(8)
           MOVE 10 TO SEQ-ELEMENT(9)
           MOVE 11 TO SEQ-ELEMENT(10)

           * Create cycle by linking element 10 to element 4
           MOVE 4 TO SEQ-ELEMENT(11)  *> This creates a cycle

           DISPLAY "Sequence: "
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 11
               DISPLAY SEQ-ELEMENT(I) " "
           END-PERFORM
           DISPLAY " "

           * Run Brent's algorithm
           CALL "BRENT-ALGORITHM" USING CURRENT-VALUE,
                                              TORTOISE,
                                              HARE,
                                              POWER,
                                              LAMBDA,
                                              CYCLE-START,
                                              FOUND-CYCLE

           IF FOUND-CYCLE = 1
               DISPLAY "Cycle detected!"
               DISPLAY "Cycle length (lambda): " LAMBDA
               DISPLAY "Starting position: " CYCLE-START
           ELSE
               DISPLAY "No cycle found"
           END-IF

           STOP RUN.

       BRENT-ALGORITHM.
           *> This is the core algorithm implementation
           *> Initialize tortoise and hare to first element
           MOVE SEQ-ELEMENT(1) TO TORTOISE
           MOVE SEQ-ELEMENT(1) TO HARE
           MOVE 1 TO POWER
           MOVE 1 TO LAMBDA
           MOVE 0 TO CYCLE-START

           PERFORM UNTIL FOUND-CYCLE = 1 OR ITERATION > MAX-ITERATIONS
               ADD 1 TO ITERATION

               *> Move hare one step
               MOVE HARE TO CURRENT-VALUE
               COMPUTE CURRENT-VALUE = SEQ-ELEMENT(CURRENT-VALUE)
               MOVE CURRENT-VALUE TO HARE

               *> Check if cycle detected
               IF TORTOISE = HARE
                   MOVE 1 TO FOUND-CYCLE
                   EXIT PERFORM
               END-IF

               *> Move hare another step (if needed)
               IF POWER = LAMBDA
                   MOVE HARE TO CURRENT-VALUE
                   COMPUTE CURRENT-VALUE = SEQ-ELEMENT(CURRENT-VALUE)
                   MOVE CURRENT-VALUE TO HARE
                   ADD 1 TO LAMBDA
                   MOVE POWER TO CYCLE-START
                   COMPUTE POWER = POWER * 2
               END-IF

               *> Move tortoise one step
               MOVE TORTOISE TO CURRENT-VALUE
               COMPUTE CURRENT-VALUE = SEQ-ELEMENT(CURRENT-VALUE)
               MOVE CURRENT-VALUE TO TORTOISE

           END-PERFORM.

       *> Alternative implementation with proper Brent's algorithm steps
       BRENT-ALGORITHM-V2.
           *> Initialize tortoise to start of sequence
           MOVE SEQ-ELEMENT(1) TO TORTOISE

           *> Phase 1: Find a point in the cycle
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > POWER OR FOUND-CYCLE = 1
               MOVE HARE TO CURRENT-VALUE
               COMPUTE CURRENT-VALUE = SEQ-ELEMENT(CURRENT-VALUE)
               MOVE CURRENT-VALUE TO HARE

               IF TORTOISE = HARE
                   MOVE 1 TO FOUND-CYCLE
                   GO TO BRENT-ALGORITHM-EXIT
               END-IF
           END-PERFORM

           *> Phase 2: Find cycle length (lambda)
           MOVE 1 TO LAMBDA
           MOVE HARE TO CURRENT-VALUE
           COMPUTE CURRENT-VALUE = SEQ-ELEMENT(CURRENT-VALUE)
           MOVE CURRENT-VALUE TO HARE

           PERFORM UNTIL CURRENT-VALUE = TORTOISE
               COMPUTE CURRENT-VALUE = SEQ-ELEMENT(CURRENT-VALUE)
               ADD 1 TO LAMBDA
           END-PERFORM

           *> Phase 3: Find starting point of cycle
           MOVE SEQ-ELEMENT(1) TO HARE
           MOVE TORTOISE TO CURRENT-VALUE

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LAMBDA
               COMPUTE CURRENT-VALUE = SEQ-ELEMENT(CURRENT-VALUE)
           END-PERFORM

           *> Reset tortoise to start of sequence
           MOVE SEQ-ELEMENT(1) TO TORTOISE

           PERFORM UNTIL TORTOISE = HARE
               COMPUTE TORTOISE = SEQ-ELEMENT(TORTOISE)
               COMPUTE HARE = SEQ-ELEMENT(HARE)
           END-PERFORM

           MOVE TORTOISE TO CYCLE-START.

       BRENT-ALGORITHM-EXIT.
           EXIT.
```

## Key Features of This Implementation:

1. **Sequence Table**: Uses an array to represent the linked list sequence
2. **Tortoise and Hare**: Implements the classic two-pointer approach
3. **Power Doubling**: Uses Brent's optimization of power doubling for cycle detection
4. **Cycle Length Calculation**: Determines the length of the detected cycle
5. **Starting Point Detection**: Identifies where the cycle begins

## Algorithm Steps:

1. **Phase 1**: Use tortoise (slow) and hare (fast) pointers to detect a cycle
2. **Phase 2**: Calculate the cycle length (lambda)
3. **Phase 3**: Find the starting position of the cycle

## Usage Notes:

- The sequence table represents a linked list where each element points to another element
- Cycle is created by having a later element point back to an earlier one
- The algorithm efficiently detects cycles in O(λ + μ) time complexity
- Memory usage is minimal with only a few working storage variables

This implementation demonstrates how Brent's cycle detection algorithm can be adapted for COBOL, maintaining the mathematical elegance of the original algorithm while using COBOL's structured approach.