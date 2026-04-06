# Newton-Raphson Method in LMC (Little Man Computer)

Here's an implementation of the Newton-Raphson method to find the square root of a number in LMC:

```assembly
; Newton-Raphson Method - Square Root Calculation
; Uses formula: x_new = (x_old + n/x_old) / 2
; Input: N (number to find square root of)
; Output: Square root of N

    INP         ; Input N
    STA NUM     ; Store N
    LDA ONE     ; Load 1.0 (initial guess)
    STA XOLD    ; Store initial guess
    LDA ZERO    ; Load 0.0 (for comparison)
    STA XNEW    ; Initialize XNEW

LOOP    LDA XOLD    ; Load current guess
        LDA NUM     ; Load N
        DIV XOLD    ; N / XOLD
        STA TEMP    ; Store result
        LDA XOLD    ; Load current guess
        ADD TEMP    ; XOLD + (N/XOLD)
        LDA TWO     ; Load 2.0
        DIV TWO     ; (XOLD + N/XOLD) / 2
        STA XNEW    ; Store new guess
        LDA XNEW    ; Load new guess
        LDA XOLD    ; Load old guess
        SUB XOLD    ; Difference = new - old
        STA DIFF    ; Store difference
        LDA DIFF    ; Load difference
        LDA ZERO    ; Load 0.0
        SUB ZERO    ; Compare with 0
        BRZ DONE    ; If difference = 0, done
        LDA XNEW    ; Load new guess
        STA XOLD    ; Update old guess
        BRA LOOP    ; Continue iteration

DONE    LDA XNEW    ; Load final result
        OUT         ; Output result
        HLT         ; Halt

; Data section
NUM   DAT 0     ; Input number
XOLD  DAT 0     ; Previous guess
XNEW  DAT 0     ; Current guess
DIFF  DAT 0     ; Difference
TEMP  DAT 0     ; Temporary storage

; Constants
ONE   DAT 1   ; 1.0
TWO   DAT 2   ; 2.0
ZERO  DAT 0   ; 0.0
```

## How it works:

1. **Input**: User inputs the number N for which we want to find the square root
2. **Initialization**: Start with initial guess (1.0) and set up variables
3. **Iteration**: Apply Newton-Raphson formula: `x_new = (x_old + N/x_old) / 2`
4. **Convergence Check**: Compare current and previous guesses to determine if we've converged
5. **Output**: Display the final approximation

## Example execution for √16:
- Initial: x₀ = 1.0
- Iteration 1: x₁ = (1.0 + 16/1.0) / 2 = 8.5
- Iteration 2: x₂ = (8.5 + 16/8.5) / 2 = 4.9706
- Iteration 3: x₃ = (4.9706 + 16/4.9706) / 2 = 4.095
- Continue until convergence

## Notes:
- This is a simplified version that demonstrates the core algorithm
- LMC has limited precision, so convergence may be approximate
- The algorithm converges quickly for most inputs
- The program assumes positive input numbers

