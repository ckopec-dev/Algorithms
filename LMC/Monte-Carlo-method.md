# Monte Carlo Method Example in LMC Assembly Language

Here's a Monte Carlo simulation to estimate π using the LMC (Little Man Computer) assembly language:

```assembly
; Monte Carlo Method to Estimate π
; Algorithm: Generate random points in unit square, count those inside unit circle
; π ≈ 4 * (points inside circle / total points)

    INP         ; Read number of iterations (n)
    STA COUNT   ; Store iterations count
    LDA ZERO    ; Initialize counters
    STA CIRCLE  ; Points inside circle
    STA TOTAL   ; Total points
    STA RAND1   ; Random number 1
    STA RAND2   ; Random number 2
    STA PI_EST  ; Estimated π

LOOP    LDA TOTAL   ; Load total count
        LDA COUNT   ; Load maximum count
        SUB ONE     ; Subtract 1
        BRZ END     ; If zero, end
        STA TOTAL   ; Store updated total

        ; Generate random numbers between 0-1
        LDA RAND1   ; Load previous random 1
        LDA RAND1   ; Load previous random 1
        ADD SEED    ; Add seed value
        MUL FACTOR  ; Multiply by factor
        MOD MODULUS ; Take modulo
        STA RAND1   ; Store new random 1

        LDA RAND2   ; Load previous random 2
        LDA RAND2   ; Load previous random 2
        ADD SEED    ; Add seed value
        MUL FACTOR  ; Multiply by factor
        MOD MODULUS ; Take modulo
        STA RAND2   ; Store new random 2

        ; Calculate distance from origin
        LDA RAND1   ; Load x coordinate
        MUL RAND1   ; x²
        STA TEMP1   ; Store x²
        LDA RAND2   ; Load y coordinate
        MUL RAND2   ; y²
        ADD TEMP1   ; x² + y²
        STA DIST    ; Store distance squared

        ; Check if point inside circle (distance² <= 1)
        LDA DIST    ; Load distance squared
        SUB ONE     ; Compare with 1
        BRP NOT_IN_CIRCLE ; If positive, outside

        ; Point inside circle
        LDA CIRCLE  ; Load circle count
        ADD ONE     ; Increment
        STA CIRCLE  ; Store back

NOT_IN_CIRCLE
        LDA TOTAL   ; Load total count
        ADD ONE     ; Increment
        STA TOTAL   ; Store back

        BRA LOOP    ; Continue loop

END     LDA CIRCLE  ; Load points inside circle
        MUL FOUR    ; Multiply by 4
        DIV TOTAL   ; Divide by total points
        STA PI_EST  ; Store estimated π
        OUT         ; Output result
        HLT         ; Halt

; Data section
COUNT   DAT 000   ; Number of iterations
CIRCLE  DAT 000   ; Points inside circle
TOTAL   DAT 000   ; Total points
RAND1   DAT 000   ; Random number 1
RAND2   DAT 000   ; Random number 2
PI_EST  DAT 000   ; Estimated π

; Constants
ZERO    DAT 000   ; Zero value
ONE     DAT 001   ; One value
FOUR    DAT 004   ; Four value
SEED    DAT 017   ; Random seed
FACTOR  DAT 003   ; Multiplication factor
MODULUS DAT 013   ; Modulo value
TEMP1   DAT 000   ; Temporary storage
DIST    DAT 000   ; Distance squared

```

## How it works:

1. **Input**: User provides number of iterations (n)
2. **Random Generation**: Uses a simple linear congruential generator to create pseudo-random numbers
3. **Point Generation**: Generates random (x,y) points in unit square [0,1] × [0,1]
4. **Circle Test**: For each point, calculates distance from origin and checks if it's inside unit circle
5. **Estimation**: π ≈ 4 × (points inside circle / total points)
6. **Output**: Displays estimated π value

## Key Monte Carlo Concepts Implemented:

- **Random Sampling**: Generates random points
- **Statistical Estimation**: Uses ratio of points inside circle to total points
- **Convergence**: More iterations yield better approximation
- **Probabilistic Method**: Uses probability theory to solve deterministic problem

The algorithm demonstrates the fundamental Monte Carlo principle of using random sampling to solve mathematical problems.

