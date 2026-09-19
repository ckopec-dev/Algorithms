# Metropolis-Hastings Algorithm in COBOL

Here's an example implementation of the Metropolis-Hastings algorithm in COBOL for sampling from a target distribution:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. METROPOLIS-HASTINGS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  RANDOM-SEED          PIC 9(8) VALUE 12345678.
       01  CURRENT-SAMPLE       PIC 9(8)V9(6).
       01  CANDIDATE-SAMPLE     PIC 9(8)V9(6).
       01  ACCEPT-PROBABILITY   PIC 9(8)V9(6).
       01  UNIFORM-RANDOM       PIC 9(8)V9(6).
       01  ACCEPTED             PIC 9 VALUE 0.
       01  ITERATION            PIC 9(6) VALUE 0.
       01  TOTAL-ITERATIONS     PIC 9(6) VALUE 10000.
       01  SAMPLE-OUTPUT        PIC 9(8)V9(6).
       01  LOG-PROBABILITY      PIC 9(8)V9(6).

       01  SAMPLE-ARRAY.
           05  SAMPLE-TABLE    OCCURS 10000 TIMES.
               10  SAMPLE-VALUE PIC 9(8)V9(6).

       PROCEDURE DIVISION.

       MAIN-PROGRAM.
           PERFORM INITIALIZE-SAMPLE
           PERFORM GENERATE-SAMPLES
           PERFORM DISPLAY-RESULTS
           STOP RUN.

       INITIALIZE-SAMPLE.
           MOVE 0 TO CURRENT-SAMPLE
           MOVE 1 TO RANDOM-SEED.

       GENERATE-SAMPLES.
           PERFORM VARYING ITERATION FROM 1 BY 1
               UNTIL ITERATION > TOTAL-ITERATIONS
               IF ITERATION = 1
                   MOVE 0 TO CANDIDATE-SAMPLE
               ELSE
                   PERFORM GENERATE-CANDIDATE
               END-IF
               PERFORM CALCULATE-ACCEPTANCE-PROBABILITY
               PERFORM ACCEPT-REJECT-DECISION
               MOVE CURRENT-SAMPLE TO SAMPLE-TABLE(ITERATION)
           END-PERFORM.

       GENERATE-CANDIDATE.
           COMPUTE CANDIDATE-SAMPLE = 
               CURRENT-SAMPLE + (RANDOM-GAUSSIAN() * 0.5).

       CALCULATE-ACCEPTANCE-PROBABILITY.
           COMPUTE LOG-PROBABILITY = 
               -0.5 * (CANDIDATE-SAMPLE * CANDIDATE-SAMPLE) +
               -0.5 * (CURRENT-SAMPLE * CURRENT-SAMPLE)
           COMPUTE ACCEPT-PROBABILITY = 
               EXP(LOG-PROBABILITY).

       ACCEPT-REJECT-DECISION.
           PERFORM GENERATE-UNIFORM-RANDOM
           IF UNIFORM-RANDOM < ACCEPT-PROBABILITY
               MOVE CANDIDATE-SAMPLE TO CURRENT-SAMPLE
               ADD 1 TO ACCEPTED
           END-IF.

       GENERATE-UNIFORM-RANDOM.
           COMPUTE RANDOM-SEED = (RANDOM-SEED * 16807) MOD 2147483647
           COMPUTE UNIFORM-RANDOM = 
               RANDOM-SEED / 2147483647.0.

       DISPLAY-RESULTS.
           DISPLAY "METROPOLIS-HASTINGS SAMPLING RESULTS"
           DISPLAY "Total iterations: " TOTAL-ITERATIONS
           DISPLAY "Accepted samples: " ACCEPTED
           DISPLAY "Acceptance rate: " 
               (ACCEPTED * 100.0 / TOTAL-ITERATIONS) "%"
           DISPLAY "First 10 samples:"
           PERFORM VARYING ITERATION FROM 1 BY 1
               UNTIL ITERATION > 10
               DISPLAY SAMPLE-TABLE(ITERATION)
           END-PERFORM.

       RANDOM-GAUSSIAN.
           01  U1                   PIC 9(8)V9(6).
           01  U2                   PIC 9(8)V9(6).
           01  Z0                   PIC 9(8)V9(6).
           01  Z1                   PIC 9(8)V9(6).
           01  PI                   PIC 9(8)V9(6) VALUE 3.14159265359.
           
           PERFORM GENERATE-UNIFORM-RANDOM
           COMPUTE U1 = UNIFORM-RANDOM
           PERFORM GENERATE-UNIFORM-RANDOM
           COMPUTE U2 = UNIFORM-RANDOM
           COMPUTE Z0 = SQRT(-2.0 * LOG(U1)) * COS(2.0 * PI * U2)
           MOVE Z0 TO RETURN-VALUE.

       END PROGRAM METROPOLIS-HASTINGS.
```

## Key Components of the Implementation:

### 1. **Data Definitions**
- `CURRENT-SAMPLE`: Current state in the Markov chain
- `CANDIDATE-SAMPLE`: Proposed new state
- `ACCEPT-PROBABILITY`: Acceptance probability for Metropolis-Hastings
- `SAMPLE-TABLE`: Array to store generated samples

### 2. **Core Algorithm Steps**
1. **Initialization**: Start with initial sample value
2. **Candidate Generation**: Propose new sample using Gaussian random walk
3. **Probability Calculation**: Compute acceptance probability using target distribution
4. **Accept/Reject Decision**: Accept or reject candidate based on probability
5. **Sampling**: Store accepted samples

### 3. **Mathematical Implementation**
- Uses standard normal distribution for proposal distribution
- Computes log-probability ratio for numerical stability
- Implements exponential function for acceptance calculation

### 4. **Random Number Generation**
- Linear congruential generator (LCG) for uniform random numbers
- Box-Muller transform for generating Gaussian random variables

This implementation demonstrates the fundamental Metropolis-Hastings algorithm in COBOL, suitable for sampling from a standard normal distribution. The algorithm will generate samples that converge to the target distribution as the number of iterations increases.