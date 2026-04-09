# Viterbi Algorithm in LMC (Little Man Computer)

Here's an implementation of the Viterbi algorithm for a simple Hidden Markov Model in LMC:

```assembly
; Viterbi Algorithm Implementation for HMM
; Simple 2-state model with 2 observations

; Constants
START   DAT 100
STATE1  DAT 101
STATE2  DAT 102
OBS1    DAT 103
OBS2    DAT 104
PROB1   DAT 105
PROB2   DAT 106
MAXPROB DAT 107
RESULT  DAT 108

; Model parameters
; Initial probabilities
INIT1   DAT 50    ; P(Start=State1) = 0.5
INIT2   DAT 50    ; P(Start=State2) = 0.5

; Transition probabilities
TRANS11 DAT 70    ; P(State1->State1) = 0.7
TRANS12 DAT 30    ; P(State1->State2) = 0.3
TRANS21 DAT 40    ; P(State2->State1) = 0.4
TRANS22 DAT 60    ; P(State2->State2) = 0.6

; Emission probabilities
EMIT11  DAT 60    ; P(Obs1|State1) = 0.6
EMIT12  DAT 40    ; P(Obs1|State2) = 0.4
EMIT21  DAT 30    ; P(Obs2|State1) = 0.3
EMIT22  DAT 70    ; P(Obs2|State2) = 0.7

; Input sequence (0 = Obs1, 1 = Obs2)
INPUT   DAT 0     ; First observation
INPUT2  DAT 1     ; Second observation
INPUT3  DAT 0     ; Third observation

; Main program
        LDA START
        STA RESULT
        LDA INIT1
        STA PROB1
        LDA INIT2
        STA PROB2

        ; Process first observation
        LDA INPUT
        BRZ PROCESS_OBS1
        LDA PROB1
        MUL EMIT21
        STA PROB1
        LDA PROB2
        MUL EMIT22
        STA PROB2
        BRZ VITERBI_END

PROCESS_OBS1
        LDA PROB1
        MUL EMIT11
        STA PROB1
        LDA PROB2
        MUL EMIT12
        STA PROB2

        ; Process second observation
        LDA INPUT2
        BRZ PROCESS_OBS2
        LDA PROB1
        MUL EMIT21
        STA PROB1
        LDA PROB2
        MUL EMIT22
        STA PROB2
        BRZ VITERBI_END

PROCESS_OBS2
        LDA PROB1
        MUL EMIT11
        STA PROB1
        LDA PROB2
        MUL EMIT12
        STA PROB2

        ; Process third observation
        LDA INPUT3
        BRZ PROCESS_OBS3
        LDA PROB1
        MUL EMIT21
        STA PROB1
        LDA PROB2
        MUL EMIT22
        STA PROB2
        BRZ VITERBI_END

PROCESS_OBS3
        LDA PROB1
        MUL EMIT11
        STA PROB1
        LDA PROB2
        MUL EMIT12
        STA PROB2

VITERBI_END
        ; Find maximum probability
        LDA PROB1
        SUB PROB2
        BRP MAX_IS_1
        LDA PROB2
        STA MAXPROB
        LDA STATE2
        STA RESULT
        BRZ END_PROGRAM

MAX_IS_1
        LDA PROB1
        STA MAXPROB
        LDA STATE1
        STA RESULT

END_PROGRAM
        HLT

; Helper routines
; Multiply routine
MULTIPLY LDA ACCUM
        ADD MULTIPLIER
        STA ACCUM
        LDA COUNTER
        SUB ONE
        STA COUNTER
        BRZ MULTIPLY_END
        BRA MULTIPLY

MULTIPLY_END
        LDA ACCUM
        STA RESULT
        BRZ END_PROGRAM

; Constants
ACCUM   DAT 0
MULTIPLIER DAT 0
COUNTER DAT 0
ONE     DAT 1

        END
```

## How the Viterbi Algorithm Works in LMC:

### Key Components:
1. **State Probabilities**: Track probability of being in each state
2. **Transition Probabilities**: Model state changes
3. **Emission Probabilities**: Likelihood of observations given states
4. **Dynamic Programming**: Update probabilities at each time step

### Algorithm Steps:
1. **Initialization**: Set initial state probabilities
2. **Recursion**: For each observation, calculate:
   - Probability of transitioning to each state
   - Probability of emitting current observation
   - Update state probabilities
3. **Termination**: Select most probable final state

### Memory Usage:
- `PROB1`, `PROB2`: Current state probabilities
- `MAXPROB`: Maximum probability found
- `RESULT`: Final state prediction
- Model parameters stored in memory locations

This implementation demonstrates the core Viterbi algorithm principles in LMC's limited instruction set, showing how dynamic programming can be implemented using basic arithmetic operations and conditional jumps.

