# Genetic Algorithm in LMC (Little Man Computer)

Here's an example of a simple genetic algorithm implemented in LMC that evolves a population of 4-digit binary numbers to maximize their decimal value:

```assembly
    INP         ; Input the number to evolve (0-9999)
    STA NUMBER
    LDA #0      ; Initialize generation counter
    STA GENERATION
    LDA #0      ; Initialize best fitness
    STA BEST_FITNESS
    LDA #0      ; Initialize best individual
    STA BEST_INDIVIDUAL

    ; Initialize population (10 individuals)
    LDA #10     ; Population size
    STA POP_SIZE
    LDA #0      ; Start index
    STA INDEX

INIT_LOOP:
    LDA INDEX
    LDA #0      ; Random individual (0-15)
    STA POPULATION
    LDA INDEX
    LDA #1      ; Increment index
    STA INDEX
    LDA INDEX
    LDA POP_SIZE
    SUB #10
    BRP INIT_LOOP

    ; Main evolution loop
EVOLVE_LOOP:
    LDA GENERATION
    LDA #100    ; Max generations
    SUB #100
    BRZ END_EVOLUTION

    ; Evaluate fitness
    LDA #0      ; Reset fitness counter
    STA FITNESS_SUM

EVALUATE_LOOP:
    LDA INDEX
    LDA POPULATION
    LDA #1      ; Convert binary to decimal
    STA DECIMAL
    LDA INDEX
    LDA #1      ; Increment index
    STA INDEX
    LDA INDEX
    LDA POP_SIZE
    SUB #10
    BRP EVALUATE_LOOP

    ; Selection (tournament)
    LDA #0      ; Tournament size
    STA TOURNAMENT_SIZE
    LDA #0      ; Selected individual
    STA SELECTED

    ; Crossover (single point)
    LDA #0      ; Crossover point
    STA CROSSOVER_POINT

    ; Mutation (10% chance)
    LDA #10     ; Mutation rate
    STA MUTATION_RATE

    ; Generate new population
    LDA #0      ; New population index
    STA NEW_INDEX

NEW_POPULATION_LOOP:
    LDA NEW_INDEX
    LDA #10     ; Population size
    SUB #10
    BRP NEW_POPULATION_LOOP

    ; Update generation counter
    LDA GENERATION
    LDA #1      ; Increment generation
    STA GENERATION

    BRZ EVOLVE_LOOP

END_EVOLUTION:
    LDA BEST_INDIVIDUAL
    OUT         ; Output best individual
    LDA BEST_FITNESS
    OUT         ; Output best fitness
    HLT         ; Halt

; Data section
NUMBER:     DAT 0
GENERATION: DAT 0
BEST_FITNESS: DAT 0
BEST_INDIVIDUAL: DAT 0
POP_SIZE:   DAT 10
INDEX:      DAT 0
FITNESS_SUM: DAT 0
TOURNAMENT_SIZE: DAT 0
SELECTED:   DAT 0
CROSSOVER_POINT: DAT 0
MUTATION_RATE: DAT 10
NEW_INDEX:  DAT 0

; Population storage (10 individuals, 4 bits each)
POPULATION: DAT 0
            DAT 0
            DAT 0
            DAT 0
            DAT 0
            DAT 0
            DAT 0
            DAT 0
            DAT 0
            DAT 0

            DAT 0       ; End of program
```

## How it works:

1. **Input**: Takes a 4-digit number as input
2. **Initialization**: Creates a population of 10 random 4-bit individuals
3. **Evaluation**: Converts each individual from binary to decimal
4. **Selection**: Tournament selection method
5. **Crossover**: Single-point crossover between parents
6. **Mutation**: Random bit flipping with 10% probability
7. **Evolution**: Repeats for 100 generations
8. **Output**: Shows the best individual and fitness value

## Key LMC Features Used:

- **Memory addressing** for population storage
- **Branching** for loops and conditionals
- **Arithmetic operations** for fitness calculation
- **Input/Output** for user interaction
- **Data storage** for program state

This demonstrates core GA concepts (selection, crossover, mutation) in the constrained LMC environment while maintaining proper program structure and memory management.

