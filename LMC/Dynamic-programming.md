# Dynamic Programming Example in LMC (Little Man Computer)

Here's a dynamic programming solution to compute the Fibonacci sequence using LMC:

## LMC Assembly Code

```
; Fibonacci Sequence Calculator (Dynamic Programming)
; Computes Fibonacci number at position N using dynamic programming

    INP         ; Read input N
    STA N       ; Store N in memory location N
    LDA ZERO    ; Load 0 into accumulator
    STA FIB_0   ; Initialize FIB_0 = 0
    LDA ONE     ; Load 1 into accumulator
    STA FIB_1   ; Initialize FIB_1 = 1
    LDA N       ; Load N again
    STA COUNT   ; Initialize loop counter

    LDA COUNT   ; Load counter
    BRZ DONE    ; If counter = 0, we're done

FIB_LOOP:
    LDA FIB_0   ; Load FIB[i-2]
    ADD FIB_1   ; Add FIB[i-1]
    STA TEMP    ; Store result in temp
    LDA FIB_1   ; Load FIB[i-1]
    STA FIB_0   ; Move FIB[i-1] to FIB[i-2]
    LDA TEMP    ; Load FIB[i]
    STA FIB_1   ; Move result to FIB[i-1]
    LDA COUNT   ; Load counter
    SUB ONE     ; Decrement counter
    STA COUNT   ; Store new counter
    BRZ DONE    ; If counter = 0, done
    BRA FIB_LOOP ; Continue loop

DONE:
    LDA FIB_1   ; Load final result
    OUT         ; Output result
    HLT         ; Halt

; Memory locations
N       DAT 0     ; Input number
COUNT   DAT 0     ; Loop counter
FIB_0   DAT 0     ; Previous Fibonacci number
FIB_1   DAT 0     ; Current Fibonacci number
TEMP    DAT 0     ; Temporary storage
ZERO    DAT 0     ; Constant 0
ONE     DAT 1     ; Constant 1
```

## How It Works

This dynamic programming algorithm computes Fibonacci numbers efficiently by:

1. **Base Cases**: Initialize FIB_0 = 0 and FIB_1 = 1
2. **Iterative Calculation**: For each step, calculate the next Fibonacci number by adding the previous two
3. **Space Optimization**: Only keep track of the last two values instead of storing all previous values
4. **Loop Control**: Use a counter to control the number of iterations

## Example Execution

For input N = 5:
- FIB_0 = 0, FIB_1 = 1 (initial)
- Iteration 1: FIB_1 = 0 + 1 = 1, FIB_0 = 1
- Iteration 2: FIB_1 = 1 + 1 = 2, FIB_0 = 1  
- Iteration 3: FIB_1 = 1 + 2 = 3, FIB_0 = 2
- Iteration 4: FIB_1 = 2 + 3 = 5, FIB_0 = 3
- Output: 5 (5th Fibonacci number)

## Dynamic Programming Benefits

- **Time Complexity**: O(n) instead of O(2^n) for naive recursion
- **Space Complexity**: O(1) - only storing two values
- **No Recursion**: Avoids stack overflow issues
- **Efficient**: Each Fibonacci number computed exactly once

This demonstrates how dynamic programming principles can be implemented even in the constrained LMC architecture.

