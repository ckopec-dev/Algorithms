# Simulated Annealing Algorithm in LMC

Here's an implementation of a simulated annealing algorithm in LMC (Little Man Computer) assembly language. This example demonstrates the core concepts of simulated annealing for optimization problems.

```assembly
; Simulated Annealing Algorithm in LMC
; This program implements a basic simulated annealing algorithm
; to find the minimum of a simple function

    INP         ; Input initial temperature
    STA TEMP    ; Store initial temperature
    INP         ; Input initial solution
    STA SOLUTION
    INP         ; Input number of iterations
    STA ITER    ; Store iteration count

    ; Initialize variables
    LDA ZERO    ; Load 0
    STA BEST    ; Initialize best solution
    STA BESTVAL ; Initialize best value
    STA COUNT   ; Initialize iteration counter
    STA TEMP    ; Initialize temperature

LOOP    LDA COUNT   ; Load iteration counter
        LDA ITER    ; Load max iterations
        SUB ONE     ; Subtract 1
        BRZ END     ; If zero, end
        LDA COUNT   ; Load counter
        ADD ONE     ; Increment counter
        STA COUNT   ; Store back

        ; Generate neighbor solution
        LDA SOLUTION
        ADD RAND    ; Add random value
        STA NEIGHBOR ; Store neighbor

        ; Calculate function value for neighbor
        LDA NEIGHBOR
        STA X       ; Store neighbor as x
        LDA X       ; Load x
        MUL X       ; Multiply x by x (x^2)
        STA FVAL    ; Store function value

        ; Calculate function value for current solution
        LDA SOLUTION
        STA X       ; Store current solution as x
        LDA X       ; Load x
        MUL X       ; Multiply x by x (x^2)
        STA CURVAL  ; Store current value

        ; Accept or reject neighbor
        LDA CURVAL  ; Load current value
        SUB FVAL    ; Subtract neighbor value
        BRP ACCEPT  ; If positive, accept

        ; Calculate probability of acceptance
        LDA TEMP    ; Load temperature
        DIV ONEHUNDRED ; Divide by 100 (simplified)
        STA PROB    ; Store probability
        LDA RAND    ; Load random number
        SUB PROB    ; Compare with probability
        BRP REJECT  ; If random > probability, reject

ACCEPT  LDA NEIGHBOR ; Accept neighbor
        STA SOLUTION ; Update solution
        LDA FVAL   ; Update best if better
        STA CURVAL ; Store current value
        LDA BESTVAL ; Load best value
        SUB CURVAL ; Compare with current
        BRP UPDATE ; If better, update

REJECT  ; Do nothing, keep current solution

UPDATE  LDA FVAL   ; Load new best value
        STA BESTVAL ; Store as best value
        LDA NEIGHBOR ; Load new best solution
        STA BEST    ; Store as best solution

        ; Cool down temperature
        LDA TEMP    ; Load temperature
        DIV COOL    ; Cool by factor
        STA TEMP    ; Store new temperature

        BRA LOOP    ; Continue loop

END     LDA BEST    ; Load best solution
        OUT         ; Output best solution
        LDA BESTVAL ; Load best value
        OUT         ; Output best value
        HLT         ; Halt

; Data section
TEMP    DAT 100     ; Initial temperature
SOLUTION DAT 5      ; Initial solution
BEST    DAT 0      ; Best solution found
BESTVAL DAT 9999   ; Best value found
COUNT   DAT 0      ; Iteration counter
ITER    DAT 100    ; Maximum iterations
NEIGHBOR DAT 0    ; Neighbor solution
FVAL    DAT 0      ; Function value
CURVAL  DAT 0      ; Current value
X       DAT 0      ; Variable x
RAND    DAT 10     ; Random value
PROB    DAT 0      ; Probability
ONE     DAT 1      ; Constant 1
ZERO    DAT 0      ; Constant 0
ONEHUNDRED DAT 100 ; Constant 100
COOL    DAT 2      ; Cooling factor
```

## Algorithm Explanation

This LMC implementation demonstrates the core principles of simulated annealing:

1. **Initialization**: Sets up initial temperature, solution, and iteration count
2. **Neighbor Generation**: Creates a neighboring solution by adding random variation
3. **Function Evaluation**: Calculates the objective function value for both current and neighbor solutions
4. **Acceptance Criteria**: 
   - If neighbor is better, accept it
   - If neighbor is worse, accept it with probability based on temperature
5. **Cooling**: Gradually reduces temperature to reduce acceptance probability
6. **Termination**: Stops after maximum iterations and outputs the best solution found

## Key Components

- **Temperature Control**: Controls the probability of accepting worse solutions
- **Randomization**: Introduces randomness in neighbor generation
- **Cooling Schedule**: Gradually reduces temperature over iterations
- **Acceptance Probability**: Uses Metropolis criterion for decision making

This implementation shows how simulated annealing can escape local optima by occasionally accepting worse solutions, ultimately converging toward a global optimum.

