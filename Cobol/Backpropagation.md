# Backpropagation Algorithm in COBOL

Here's an example of a simple backpropagation algorithm implemented in COBOL for a neural network with one hidden layer:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BACKPROPAGATION.
       AUTHOR. AI LEARNING EXAMPLE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  NETWORK-CONSTANTS.
           05  LEARNING-RATE          PIC 9V99 VALUE 0.5.
           05  INPUT-SIZE             PIC 99 VALUE 2.
           05  HIDDEN-SIZE            PIC 99 VALUE 3.
           05  OUTPUT-SIZE            PIC 99 VALUE 1.
           05  MAX-ITERATIONS         PIC 999 VALUE 1000.

       01  NETWORK-WEIGHTS.
           05  INPUT-HIDDEN-WT        OCCURS 2 3 TIMES PIC 9V999.
           05  HIDDEN-OUTPUT-WT       OCCURS 3 1 TIMES PIC 9V999.

       01  NETWORK-BIASES.
           05  HIDDEN-BIAS            OCCURS 3 TIMES PIC 9V999.
           05  OUTPUT-BIAS            PIC 9V999.

       01  NETWORK-VALUES.
           05  INPUT-VALUES           OCCURS 2 TIMES PIC 9V999.
           05  HIDDEN-VALUES          OCCURS 3 TIMES PIC 9V999.
           05  OUTPUT-VALUES          PIC 9V999.
           05  TARGET-VALUES          PIC 9V999.

       01  DERIVATIVES.
           05  OUTPUT-ERROR           PIC 9V999.
           05  HIDDEN-ERROR           OCCURS 3 TIMES PIC 9V999.
           05  HIDDEN-DELTA           OCCURS 3 TIMES PIC 9V999.
           05  OUTPUT-DELTA           PIC 9V999.

       01  TEMPORARY-VARIABLES.
           05  SUM                    PIC 9V999.
           05  ACTIVATION             PIC 9V999.
           05  ERROR                  PIC 9V999.
           05  ITERATION              PIC 999.
           05  I                      PIC 99.
           05  J                      PIC 99.
           05  K                      PIC 99.
           05  MSE                    PIC 9V999 VALUE 0.

       01  TRAINING-DATA.
           05  TRAINING-INPUTS        OCCURS 4 2 TIMES PIC 9V999.
           05  TRAINING-TARGETS       OCCURS 4 TIMES PIC 9V999.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-WEIGHTS
           PERFORM INITIALIZE-BIASES
           PERFORM LOAD-TRAINING-DATA
           PERFORM TRAIN-NEURAL-NETWORK
           PERFORM DISPLAY-RESULTS
           STOP RUN.

       INITIALIZE-WEIGHTS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > INPUT-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > HIDDEN-SIZE
                   COMPUTE INPUT-HIDDEN-WT(I-J) = FUNCTION RANDOM * 2 - 1
               END-PERFORM
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > HIDDEN-SIZE
               COMPUTE HIDDEN-OUTPUT-WT(I-1) = FUNCTION RANDOM * 2 - 1
           END-PERFORM.

       INITIALIZE-BIASES.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > HIDDEN-SIZE
               COMPUTE HIDDEN-BIAS(I) = FUNCTION RANDOM * 2 - 1
           END-PERFORM
           COMPUTE OUTPUT-BIAS = FUNCTION RANDOM * 2 - 1.

       LOAD-TRAINING-DATA.
           MOVE 0.0 TO TRAINING-INPUTS(1-1)  MOVE 0.0 TO TRAINING-INPUTS(1-2)
           MOVE 0.0 TO TRAINING-TARGETS(1)
           MOVE 0.0 TO TRAINING-INPUTS(2-1)  MOVE 1.0 TO TRAINING-INPUTS(2-2)
           MOVE 1.0 TO TRAINING-TARGETS(2)
           MOVE 1.0 TO TRAINING-INPUTS(3-1)  MOVE 0.0 TO TRAINING-INPUTS(3-2)
           MOVE 1.0 TO TRAINING-TARGETS(3)
           MOVE 1.0 TO TRAINING-INPUTS(4-1)  MOVE 1.0 TO TRAINING-INPUTS(4-2)
           MOVE 0.0 TO TRAINING-TARGETS(4).

       TRAIN-NEURAL-NETWORK.
           PERFORM VARYING ITERATION FROM 1 BY 1 UNTIL ITERATION > MAX-ITERATIONS
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
                   PERFORM FORWARD-PASS
                   PERFORM BACKWARD-PASS
                   PERFORM UPDATE-WEIGHTS
                   COMPUTE MSE = MSE + (OUTPUT-VALUES - TRAINING-TARGETS(I)) ** 2
               END-PERFORM
               COMPUTE MSE = MSE / 4
               IF MSE < 0.001 THEN
                   DISPLAY "Converged at iteration: " ITERATION
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       FORWARD-PASS.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > HIDDEN-SIZE
               COMPUTE SUM = 0
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > INPUT-SIZE
                   COMPUTE SUM = SUM + INPUT-VALUES(I) * INPUT-HIDDEN-WT(I-J)
               END-PERFORM
               COMPUTE HIDDEN-VALUES(J) = 1 / (1 + FUNCTION EXP(-SUM - HIDDEN-BIAS(J)))
           END-PERFORM

           COMPUTE SUM = 0
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > HIDDEN-SIZE
               COMPUTE SUM = SUM + HIDDEN-VALUES(I) * HIDDEN-OUTPUT-WT(I-1)
           END-PERFORM
           COMPUTE OUTPUT-VALUES = 1 / (1 + FUNCTION EXP(-SUM - OUTPUT-BIAS)).

       BACKWARD-PASS.
           COMPUTE OUTPUT-ERROR = OUTPUT-VALUES - TARGET-VALUES
           COMPUTE OUTPUT-DELTA = OUTPUT-ERROR * OUTPUT-VALUES * (1 - OUTPUT-VALUES)

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > HIDDEN-SIZE
               COMPUTE HIDDEN-ERROR(I) = HIDDEN-OUTPUT-WT(I-1) * OUTPUT-DELTA
               COMPUTE HIDDEN-DELTA(I) = HIDDEN-ERROR(I) * HIDDEN-VALUES(I) * (1 - HIDDEN-VALUES(I))
           END-PERFORM.

       UPDATE-WEIGHTS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > HIDDEN-SIZE
               COMPUTE HIDDEN-OUTPUT-WT(I-1) = HIDDEN-OUTPUT-WT(I-1) - 
                   LEARNING-RATE * OUTPUT-DELTA * HIDDEN-VALUES(I)
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > HIDDEN-SIZE
               COMPUTE HIDDEN-BIAS(I) = HIDDEN-BIAS(I) - LEARNING-RATE * HIDDEN-DELTA(I)
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > INPUT-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > HIDDEN-SIZE
                   COMPUTE INPUT-HIDDEN-WT(I-J) = INPUT-HIDDEN-WT(I-J) - 
                       LEARNING-RATE * HIDDEN-DELTA(J) * INPUT-VALUES(I)
               END-PERFORM
           END-PERFORM

           COMPUTE OUTPUT-BIAS = OUTPUT-BIAS - LEARNING-RATE * OUTPUT-DELTA.

       DISPLAY-RESULTS.
           DISPLAY "Training completed."
           DISPLAY "Final MSE: " MSE
           DISPLAY "Final weights:"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > INPUT-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > HIDDEN-SIZE
                   DISPLAY "Input-Hidden(" I "," J "): " INPUT-HIDDEN-WT(I-J)
               END-PERFORM
           END-PERFORM
           DISPLAY "Hidden-Output weights:"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > HIDDEN-SIZE
               DISPLAY "Hidden-Output(" I "): " HIDDEN-OUTPUT-WT(I-1)
           END-PERFORM
           DISPLAY "Final bias values:"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > HIDDEN-SIZE
               DISPLAY "Hidden bias(" I "): " HIDDEN-BIAS(I)
           END-PERFORM
           DISPLAY "Output bias: " OUTPUT-BIAS.
```

## Key Components of This COBOL Backpropagation Implementation:

1. **Network Structure**: 
   - 2-input layer
   - 3-hidden layer
   - 1-output layer

2. **Core Functions**:
   - `INITIALIZE-WEIGHTS`: Random initialization of weights
   - `FORWARD-PASS`: Propagates inputs forward through network
   - `BACKWARD-PASS`: Calculates error gradients
   - `UPDATE-WEIGHTS`: Adjusts weights using gradient descent
   - `TRAIN-NEURAL-NETWORK`: Main training loop

3. **Mathematical Operations**:
   - Sigmoid activation function
   - Error calculation
   - Gradient computation
   - Weight updates

4. **Training Data**:
   - XOR problem dataset
   - Iterative training with convergence detection

This example demonstrates how neural network backpropagation can be implemented in COBOL, though it's worth noting that COBOL is not typically used for machine learning applications due to its verbosity and lack of modern numerical computing libraries.

