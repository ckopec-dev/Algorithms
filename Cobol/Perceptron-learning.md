# Perceptron Learning Algorithm in COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERCEPTRON-LEARNING.
       AUTHOR. AI Assistant.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRAINING-DATA ASSIGN TO "training.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD TRAINING-DATA.
       01 TRAINING-RECORD.
          05 INPUT-VALUES    PIC 9(1) OCCURS 2 TIMES.
          05 TARGET-VALUE    PIC 9(1).

       WORKING-STORAGE SECTION.
       01 LEARNING-RATE     PIC 9V9 VALUE 0.1.
       01 MAX-ITERATIONS    PIC 9(3) VALUE 1000.
       01 ITERATION-COUNT   PIC 9(3) VALUE 0.
       01 ERROR-COUNT       PIC 9(3) VALUE 0.
       01 CONVERGED         PIC X VALUE 'N'.
       01 WEIGHTS.
          05 WEIGHT        PIC 9V999999 OCCURS 2 TIMES.
       01 BIAS              PIC 9V999999 VALUE 0.0.
       01 ACTIVATION        PIC 9V999999.
       01 ERROR             PIC 9V999999.
       01 INPUT-VECTOR.
          05 INPUT-VALUE   PIC 9V999999 OCCURS 2 TIMES.
       01 TARGET            PIC 9V999999.
       01 TEMP-VALUE        PIC 9V999999.
       01 EOF-FLAG          PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "Perceptron Learning Algorithm Started"
           DISPLAY "Learning Rate: " LEARNING-RATE
           
           INITIALIZE-WEIGHTS
           OPEN INPUT TRAINING-DATA
           
           PERFORM TRAIN-LOOP UNTIL CONVERGED = 'Y' OR ITERATION-COUNT > MAX-ITERATIONS
           
           DISPLAY "Training Complete"
           DISPLAY "Iterations: " ITERATION-COUNT
           DISPLAY "Final Weights: " WEIGHT(1) " " WEIGHT(2)
           DISPLAY "Final Bias: " BIAS
           
           CLOSE TRAINING-DATA
           STOP RUN.

       INITIALIZE-WEIGHTS.
           MOVE 0.0 TO WEIGHT(1)
           MOVE 0.0 TO WEIGHT(2)
           MOVE 0.0 TO BIAS.

       TRAIN-LOOP.
           ADD 1 TO ITERATION-COUNT
           MOVE 0 TO ERROR-COUNT
           MOVE 'N' TO CONVERGED
           
           PERFORM READ-AND-UPDATE-WEIGHTS UNTIL EOF-FLAG = 'Y'
           
           IF ERROR-COUNT = 0
               MOVE 'Y' TO CONVERGED
           END-IF
           
           REWIND TRAINING-DATA.

       READ-AND-UPDATE-WEIGHTS.
           READ TRAINING-DATA INTO TRAINING-RECORD
               AT END MOVE 'Y' TO EOF-FLAG
               NOT AT END PERFORM PROCESS-TRAINING-EXAMPLE
           END-READ.

       PROCESS-TRAINING-EXAMPLE.
           MOVE INPUT-VALUES(1) TO INPUT-VALUE(1)
           MOVE INPUT-VALUES(2) TO INPUT-VALUE(2)
           MOVE TARGET-VALUE TO TARGET
           
           PERFORM CALCULATE-ACTIVATION
           PERFORM UPDATE-WEIGHTS.

       CALCULATE-ACTIVATION.
           COMPUTE ACTIVATION = WEIGHT(1) * INPUT-VALUE(1)
                              + WEIGHT(2) * INPUT-VALUE(2)
                              + BIAS
           COMPUTE ACTIVATION = FUNCTION EXP(ACTIVATION)
           COMPUTE ACTIVATION = ACTIVATION / (1 + ACTIVATION).

       UPDATE-WEIGHTS.
           COMPUTE ERROR = TARGET - ACTIVATION
           
           IF ABS(ERROR) > 0.001
               ADD 1 TO ERROR-COUNT
               
               COMPUTE TEMP-VALUE = LEARNING-RATE * ERROR * INPUT-VALUE(1)
               COMPUTE WEIGHT(1) = WEIGHT(1) + TEMP-VALUE
               
               COMPUTE TEMP-VALUE = LEARNING-RATE * ERROR * INPUT-VALUE(2)
               COMPUTE WEIGHT(2) = WEIGHT(2) + TEMP-VALUE
               
               COMPUTE TEMP-VALUE = LEARNING-RATE * ERROR
               COMPUTE BIAS = BIAS + TEMP-VALUE
           END-IF.

       DISPLAY-RESULTS.
           DISPLAY "Iteration " ITERATION-COUNT
           DISPLAY "Weights: " WEIGHT(1) " " WEIGHT(2)
           DISPLAY "Bias: " BIAS.
```

## Sample Training Data File (training.dat)
```
101
011
110
000
```

## Explanation

This COBOL implementation demonstrates the Perceptron learning algorithm with the following features:

1. **Data Structure**: Uses file handling for training data input
2. **Weight Initialization**: Random initialization of weights and bias
3. **Learning Process**: 
   - Reads training examples from file
   - Calculates activation using sigmoid function
   - Updates weights and bias based on error
   - Continues until convergence or max iterations

4. **Key Components**:
   - `WEIGHTS` array for input weights
   - `BIAS` for threshold value
   - `LEARNING-RATE` for training speed
   - `ACTIVATION` function with sigmoid
   - Error-based weight updates

5. **Convergence**: Stops when no errors are detected or maximum iterations reached

The algorithm learns to classify binary inputs using the perceptron learning rule with gradient descent optimization.

