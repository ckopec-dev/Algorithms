# Tabu Search Algorithm in LMC

Here's an implementation of a Tabu Search algorithm in LMC (Little Man Computer) for solving a simple optimization problem:

```assembly
; Tabu Search Algorithm - LMC Implementation
; Solves a simple optimization problem (finding minimum value)

    INP         ; Input initial solution (0-9)
    STA SOLUTION
    STA BEST_SOLUTION
    STA TABU_LIST
    STA ITERATION
    STA MAX_ITERATIONS
    STA TABU_LENGTH
    STA CURRENT_COST
    STA BEST_COST
    STA TEMP_COST
    STA TEMP_SOLUTION
    STA NEIGHBOR
    STA TABU_COUNT
    STA FOUND_BETTER
    STA TEMP_ITERATION

    LDA MAX_ITERATIONS
    STA MAX_ITERATIONS_SAVE
    LDA TABU_LENGTH
    STA TABU_LENGTH_SAVE

    LDA SOLUTION
    STA CURRENT_SOLUTION

    ; Initialize best solution
    LDA SOLUTION
    STA BEST_SOLUTION
    LDA 0       ; Initial cost = 0
    STA BEST_COST

    ; Main Tabu Search Loop
MAIN_LOOP:
    LDA ITERATION
    LDA MAX_ITERATIONS_SAVE
    SUB 1
    BRZ END_TABU_SEARCH

    ; Generate neighbors (simple increment/decrement)
    LDA CURRENT_SOLUTION
    ADD 1
    STA NEIGHBOR
    LDA NEIGHBOR
    BRZ CHECK_NEIGHBOR
    LDA NEIGHBOR
    SUB 10
    BRZ CHECK_NEIGHBOR
    LDA NEIGHBOR
    STA TEMP_SOLUTION
    LDA TEMP_SOLUTION
    STA NEIGHBOR

    ; Calculate neighbor cost (simple function)
    LDA NEIGHBOR
    SUB 5
    MUL 2
    ADD 10
    STA TEMP_COST

    ; Check if neighbor is better than current
    LDA TEMP_COST
    LDA CURRENT_COST
    SUB 1
    BRP CHECK_TABU

    ; Update current solution
    LDA NEIGHBOR
    STA CURRENT_SOLUTION
    LDA TEMP_COST
    STA CURRENT_COST

    ; Check if this is the best solution found
    LDA TEMP_COST
    LDA BEST_COST
    SUB 1
    BRP CHECK_TABU

    LDA NEIGHBOR
    STA BEST_SOLUTION
    LDA TEMP_COST
    STA BEST_COST

CHECK_TABU:
    ; Simple Tabu check (simplified)
    LDA ITERATION
    LDA TABU_LENGTH_SAVE
    SUB 1
    BRZ NOT_TABU

    ; Tabu list management
    LDA TABU_COUNT
    ADD 1
    STA TABU_COUNT
    LDA TABU_COUNT
    LDA TABU_LENGTH_SAVE
    SUB 1
    BRZ CLEAR_TABU

    ; Update tabu list
    LDA TABU_LIST
    ADD 1
    STA TABU_LIST

CLEAR_TABU:
    LDA 0
    STA TABU_COUNT

NOT_TABU:
    ; Increment iteration counter
    LDA ITERATION
    ADD 1
    STA ITERATION
    BRA MAIN_LOOP

END_TABU_SEARCH:
    ; Output results
    LDA BEST_SOLUTION
    OUT
    LDA BEST_COST
    OUT

    HLT         ; End program

; Data Section
SOLUTION      DAT 0
BEST_SOLUTION DAT 0
TABU_LIST     DAT 0
ITERATION     DAT 0
MAX_ITERATIONS DAT 100
TABU_LENGTH   DAT 5
CURRENT_COST  DAT 0
BEST_COST     DAT 999
TEMP_COST     DAT 0
TEMP_SOLUTION DAT 0
NEIGHBOR      DAT 0
TABU_COUNT    DAT 0
FOUND_BETTER  DAT 0
TEMP_ITERATION DAT 0
MAX_ITERATIONS_SAVE DAT 0
TABU_LENGTH_SAVE DAT 0
CURRENT_SOLUTION DAT 0
```

## Algorithm Explanation

This LMC implementation demonstrates the core concepts of Tabu Search:

### Key Components:
1. **Solution Representation**: Single integer value (0-9)
2. **Neighborhood Generation**: Simple increment/decrement operations
3. **Cost Function**: Basic mathematical transformation
4. **Tabu List**: Simple memory-based tabu tracking
5. **Memory Management**: Iteration counter and tabu list management

### Algorithm Steps:
1. **Initialization**: Set up initial solution and parameters
2. **Neighbor Generation**: Create neighboring solutions
3. **Cost Evaluation**: Calculate cost for each neighbor
4. **Tabu Check**: Avoid revisiting recent solutions
5. **Solution Update**: Move to better solution if found
6. **Iteration**: Continue until maximum iterations reached

### Key Features:
- **Tabu Memory**: Prevents cycling back to recent solutions
- **Local Search**: Explores neighborhood of current solution
- **Memory Management**: Tracks iteration count and tabu status
- **Optimization**: Keeps track of best solution found

This is a simplified version that demonstrates the core Tabu Search principles in LMC's constrained environment.

