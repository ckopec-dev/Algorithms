# Genetic Algorithm in COBOL

Here's a simple example of a genetic algorithm implemented in COBOL for solving a basic optimization problem:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GENETIC-ALGORITHM.
       AUTHOR. AI-ASSISTANT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTPUT-FILE ASSIGN TO "RESULTS.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD    PIC X(80).

       WORKING-STORAGE SECTION.
       01 POPULATION-SIZE   PIC 9(3) VALUE 50.
       01 CHROMOSOME-LENGTH PIC 9(3) VALUE 10.
       01 GENERATIONS       PIC 9(3) VALUE 100.
       01 MUTATION-RATE     PIC 9V9 VALUE 0.05.
       01 CROSSOVER-RATE    PIC 9V9 VALUE 0.8.
       01 BEST-FITNESS      PIC 9(5) VALUE 0.
       01 TOTAL-FITNESS     PIC 9(6) VALUE 0.
       01 RANDOM-VALUE      PIC 9V9 VALUE 0.

       01 POPULATION.
           05 INDIVIDUAL OCCURS 50 TIMES.
               10 GENE-ARRAY.
                   15 GENE-VALUE PIC 9 OCCURS 10 TIMES.
               10 FITNESS-VALUE PIC 9(5).
               10 ACCUMULATED-FITNESS PIC 9(6).

       01 TEMP-POPULATION.
           05 TEMP-INDIVIDUAL OCCURS 50 TIMES.
               10 TEMP-GENE-ARRAY.
                   15 TEMP-GENE-VALUE PIC 9 OCCURS 10 TIMES.
               10 TEMP-FITNESS PIC 9(5).

       01 SELECTION-INDEXES.
           05 SELECTED-INDEX PIC 9(3) OCCURS 50 TIMES.

       01 RNG-SEED          PIC 9(9) VALUE 123456789.
       01 RNG-VALUE         PIC 9(9) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-POPULATION
           PERFORM RUN-GA
           PERFORM DISPLAY-RESULTS
           STOP RUN.

       INITIALIZE-POPULATION.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > POPULATION-SIZE
               PERFORM INITIALIZE-INDIVIDUAL(I)
           END-PERFORM.

       INITIALIZE-INDIVIDUAL.
           01 I PIC 9(3) VALUE 1.
           MOVE I TO I
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > CHROMOSOME-LENGTH
               COMPUTE RNG-VALUE = FUNCTION MOD(RNG-SEED * 1103515245 + 12345, 2147483648)
               COMPUTE RNG-SEED = RNG-VALUE
               COMPUTE RNG-VALUE = FUNCTION MOD(RNG-VALUE, 2)
               MOVE RNG-VALUE TO GENE-VALUE(J OF I)
           END-PERFORM
           PERFORM CALCULATE-FITNESS(I).

       CALCULATE-FITNESS.
           01 I PIC 9(3) VALUE 1.
           MOVE I TO I
           COMPUTE FITNESS-VALUE(I) = 0
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > CHROMOSOME-LENGTH
               ADD GENE-VALUE(J OF I) TO FITNESS-VALUE(I)
           END-PERFORM.

       RUN-GA.
           PERFORM VARYING G FROM 1 BY 1 UNTIL G > GENERATIONS
               PERFORM CALCULATE-ACCUMULATED-FITNESS
               PERFORM SELECTION
               PERFORM CROSSOVER
               PERFORM MUTATION
               PERFORM CALCULATE-BEST-FITNESS
               DISPLAY "Generation " G " Best Fitness: " BEST-FITNESS
           END-PERFORM.

       CALCULATE-ACCUMULATED-FITNESS.
           MOVE 0 TO TOTAL-FITNESS
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > POPULATION-SIZE
               ADD FITNESS-VALUE(I) TO TOTAL-FITNESS
               IF I = 1
                   MOVE FITNESS-VALUE(I) TO ACCUMULATED-FITNESS(I)
               ELSE
                   COMPUTE ACCUMULATED-FITNESS(I) = 
                       ACCUMULATED-FITNESS(I - 1) + FITNESS-VALUE(I)
               END-IF
           END-PERFORM.

       SELECTION.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > POPULATION-SIZE
               COMPUTE RNG-VALUE = FUNCTION MOD(RNG-SEED * 1103515245 + 12345, 2147483648)
               COMPUTE RNG-SEED = RNG-VALUE
               COMPUTE RANDOM-VALUE = RNG-VALUE / 2147483648
               COMPUTE RANDOM-VALUE = RANDOM-VALUE * TOTAL-FITNESS
               PERFORM SELECT-BEST-INDIVIDUAL(RANDOM-VALUE)
           END-PERFORM.

       SELECT-BEST-INDIVIDUAL.
           01 RANDOM-FITNESS PIC 9(6) VALUE 0.
           MOVE RANDOM-FITNESS TO RANDOM-FITNESS
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > POPULATION-SIZE
               IF RANDOM-FITNESS <= ACCUMULATED-FITNESS(J)
                   MOVE J TO SELECTED-INDEX(I)
                   GO TO SELECT-BEST-INDIVIDUAL-EXIT
               END-IF
           END-PERFORM
           MOVE POPULATION-SIZE TO SELECTED-INDEX(I)
           SELECT-BEST-INDIVIDUAL-EXIT.

       CROSSOVER.
           PERFORM VARYING I FROM 1 BY 2 UNTIL I > POPULATION-SIZE
               COMPUTE RNG-VALUE = FUNCTION MOD(RNG-SEED * 1103515245 + 12345, 2147483648)
               COMPUTE RNG-SEED = RNG-VALUE
               COMPUTE RANDOM-VALUE = RNG-VALUE / 2147483648
               IF RANDOM-VALUE < CROSSOVER-RATE
                   PERFORM DO-CROSSOVER(I, I + 1)
               END-IF
           END-PERFORM.

       DO-CROSSOVER.
           01 IND1 PIC 9(3) VALUE 1.
           01 IND2 PIC 9(3) VALUE 1.
           MOVE IND1 TO IND1
           MOVE IND2 TO IND2
           COMPUTE RNG-VALUE = FUNCTION MOD(RNG-SEED * 1103515245 + 12345, 2147483648)
           COMPUTE RNG-SEED = RNG-VALUE
           COMPUTE RNG-VALUE = FUNCTION MOD(RNG-VALUE, CHROMOSOME-LENGTH)
           COMPUTE RNG-VALUE = RNG-VALUE + 1
           PERFORM VARYING J FROM RNG-VALUE BY 1 UNTIL J > CHROMOSOME-LENGTH
               MOVE GENE-VALUE(J OF IND1) TO TEMP-GENE-VALUE(J OF IND1)
               MOVE GENE-VALUE(J OF IND2) TO GENE-VALUE(J OF IND1)
               MOVE TEMP-GENE-VALUE(J OF IND1) TO GENE-VALUE(J OF IND2)
           END-PERFORM.

       MUTATION.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > POPULATION-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > CHROMOSOME-LENGTH
                   COMPUTE RNG-VALUE = FUNCTION MOD(RNG-SEED * 1103515245 + 12345, 2147483648)
                   COMPUTE RNG-SEED = RNG-VALUE
                   COMPUTE RANDOM-VALUE = RNG-VALUE / 2147483648
                   IF RANDOM-VALUE < MUTATION-RATE
                       COMPUTE RNG-VALUE = FUNCTION MOD(RNG-SEED * 1103515245 + 12345, 2147483648)
                       COMPUTE RNG-SEED = RNG-VALUE
                       COMPUTE RNG-VALUE = FUNCTION MOD(RNG-VALUE, 2)
                       MOVE RNG-VALUE TO GENE-VALUE(J OF I)
                   END-IF
               END-PERFORM
           END-PERFORM.

       CALCULATE-BEST-FITNESS.
           MOVE 0 TO BEST-FITNESS
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > POPULATION-SIZE
               IF FITNESS-VALUE(I) > BEST-FITNESS
                   MOVE FITNESS-VALUE(I) TO BEST-FITNESS
               END-IF
           END-PERFORM.

       DISPLAY-RESULTS.
           OPEN OUTPUT OUTPUT-FILE
           MOVE "GENETIC ALGORITHM RESULTS" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "Best Fitness: " TO OUTPUT-RECORD
           STRING OUTPUT-RECORD BEST-FITNESS DELIMITED BY SIZE
           WRITE OUTPUT-RECORD
           CLOSE OUTPUT-FILE.
```

## Key Components Explained:

### 1. **Data Structures**
- Population array with 50 individuals
- Each individual has 10 genes (binary values 0 or 1)
- Fitness tracking for each individual

### 2. **Main GA Operations**
- **Initialization**: Random population creation
- **Selection**: Roulette wheel selection based on fitness
- **Crossover**: Single-point crossover with 80% probability
- **Mutation**: Bit-flip mutation with 5% probability
- **Evaluation**: Fitness calculation for each individual

### 3. **Algorithm Flow**
1. Initialize random population
2. For each generation:
   - Calculate fitness for all individuals
   - Select parents for reproduction
   - Apply crossover and mutation
   - Evaluate new population
3. Display final results

### 4. **Key Features**
- Modular design with separate procedures
- Random number generation for selection and mutation
- Fitness-based selection mechanism
- Standard genetic operators (selection, crossover, mutation)

This implementation demonstrates the core principles of genetic algorithms in COBOL, though it's simplified for educational purposes. Real-world applications would require more sophisticated fitness functions and additional optimization techniques.

